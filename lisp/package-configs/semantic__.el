(require 'cc-mode)
(require 'ede)
(require 'ede/cpp-root)
(require 'semantic)
(require 'semantic/idle)
(require 'semantic/symref)
(require 'semantic/symref/grep)
(require 'semantic/symref/list)
(require 'semantic/symref/filter)
(require 'semantic/doc)
(require 'semantic/ia)
(require 'semantic/chart)
(require 'srecode)
(require 'srecode/fields)
(require 'json)
(require 'seq)
(require 'company-semantic)
(load "custom-semantic-db-grep.el")

(global-semanticdb-minor-mode t)
(global-semantic-idle-scheduler-mode t)
(global-semantic-mru-bookmark-mode t)
(global-semantic-highlight-edits-mode t)
(global-semantic-idle-summary-mode t)
(global-semantic-highlight-func-mode t)
(global-semantic-decoration-mode t)
(global-semantic-idle-breadcrumbs-mode t)
;; idle breadcrumbs are better, but are in conflict
;; with stickyfunc, so its disabled.
(global-semantic-stickyfunc-mode -1)
(global-semantic-show-unmatched-syntax-mode -1)

(setq-default semantic-idle-breadcrumbs-format-tag-function
              'semantic-format-tag-summarize)
(setq-default semantic-idle-work-parse-neighboring-files-flag nil)
(setq-default semantic-idle-work-update-headers-flag nil)
(setq-default semantic-complete-inline-analyzer-displayor-class
              'semantic-displayor-tooltip)
(setq-default semantic-edits-verbose-flag t)
(setq-default senator-step-at-tag-classes nil)
(setq-default senator-step-at-start-end-tag-classes
              '(function))
(setq-default speedbar-use-images nil)
(setq-default speedbar-use-imenu-flag t)
;; Use (semantic-symref-detect-symref-tool) for autodetecting symref
;; tool. Currently, we dont want to use that. Grep is by far the best
;; option If you have modern CPU.
;; Anyway, global is kinda buggy and cscope is not very used.
(setq-default semantic-symref-tool 'grep)

(semanticdb-enable-grep-databases)

(setq semantic-new-buffer-setup-functions
      (loop for e in semantic-new-buffer-setup-functions
            if (not (or (equal (car e) 'python-mode)
                        (equal (car e) 'js-mode)))
            collect e))

(add-to-list 'semantic-new-buffer-setup-functions
             '(emacs-lisp-mode . semantic-default-elisp-setup))
(add-to-list 'semantic-inhibit-functions
             (lambda ()
               (not (or (equal major-mode 'c-mode)
                        (equal major-mode 'c++-mode)
                        (equal major-mode 'emacs-lisp-mode)))))

(advice-add 'save-buffer :after
            (lambda (&rest args)
              (if (or (equal major-mode 'c-mode)
                      (equal major-mode 'c++-mode)
                      (equal major-mode 'emacs-lisp-mode))
                  (save-mark-and-excursion
                    (semantic-force-refresh)))))

(global-ede-mode 1)
(semantic-mode 1)



;; This function needs to be stubbed in order to prevent deletion
;; of lots of files loaded during symref searching. Whole load
;; process is very slow, so its good idea to just cache files.
(defun semantic-symref-cleanup-recent-buffers-fcn ())

;; Hack used for company complete c headers. EDE is not able to return
;; system headers without this.
(cl-defmethod ede-include-path ((this ede-cpp-root-project))
  "Get the system include path used by project THIS."
  (oref this include-path))

;; Hack used for company complete c headers. EDE is not able to return
;; system headers without this.
(cl-defmethod ede-include-path ((this ede-cpp-root-target))
  "Get the system include path used by target THIS."
  (ede-include-path (ede-target-parent this)))



;; By default, which function will trigger reparsing of current file
;; by semantic, that is very slow so we will use shortcut.
(advice-add #'which-function
            :around
            (lambda (oldfn &rest args)
              (if (or (equal major-mode 'c-mode)
                      (equal major-mode 'c++-mode))
                  (let ((fun-name (custom/semantic/get-current-function-name)))
                    (if fun-name
                        (concat fun-name "()")
                      nil))
                (apply oldfn args))))

;; Speed hack, semanticdb-fast-strip-find-results is a lot faster than
;; semanticdb-strip-find-results which caused company to lag.
(defun custom/company-semantic-completions-raw (prefix)
  (setq company-semantic--current-tags nil)
  (dolist (tag (if (and (fboundp 'semanticdb-minor-mode-p)
                        (semanticdb-minor-mode-p))
                   ;; Search the database & concatenate all matches together.
                   (semanticdb-fast-strip-find-results
                    (semanticdb-find-tags-for-completion prefix))
                 ;; Search just this file because there is no DB available.
                 (semantic-find-tags-for-completion
                  prefix (current-buffer))))
    (unless (eq (semantic-tag-class tag) 'include)
      (push tag company-semantic--current-tags)))
  (delete "" (mapcar 'semantic-tag-name company-semantic--current-tags)))
(advice-add #'company-semantic-completions-raw
            :around
            (lambda (oldfn &rest args)
              (apply #'custom/company-semantic-completions-raw args)))

;; Disable grep db for all company queries. We need just prototypes for it.
(advice-add #'company-semantic
            :around
            (lambda (oldfn &rest args)
              (let ((*custom/semantic/grep-db-enabled* nil))
                (apply oldfn args))))
(advice-add #'company-complete-common
            :around
            (lambda (oldfn &rest args)
              (let ((*custom/semantic/grep-db-enabled* nil))
                (apply oldfn args))))

;; Hack done in order to be able to complete enums. By default, no deep
;; search was done in contextless searches so I introduced partial deep
;; search only for enums.
(cl-defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-abstract-table)
   prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-for-completion prefix
                                     (custom/semantic/flatten-enum-tags-table
                                      (or tags (semanticdb-get-tags table)))))



(defun custom/semantic/diagnostic-visualizations ()
  (interactive)
  (let ((decision (ido-completing-read "Select thing to visualize: "
                                       '("Scope analyzer overhead"
                                         "Database size"
                                         "Tag complexity"
                                         "Tags by class"))))
    (cond
     ((equal decision "Scope analyzer overhead")
      (semantic-chart-analyzer))
     ((equal decision "Database size")
      (semantic-chart-database-size))
     ((equal decision "Tag complexity")
      (semantic-chart-tag-complexity))
     ((equal decision "Tags by class")
      (semantic-chart-tags-by-class)))))

(defun custom/ede/load-config-file (proj-root)
  (let ((config-path (concat proj-root
                             "emacs-project-config.json"))
        (config nil))
    (if (file-exists-p config-path)
        (let ((json-object-type 'hash-table)
              (json-array-type 'list)
              (json-key-type 'string))
          (setq config (json-read-from-string
                        (f-read-text config-path
                                     'utf-8)))))
    (if (hash-table-p config)
        (progn
          (if (and (not (null (custom/map/get "local-includes" config)))
                   (not (listp (custom/map/get "local-includes" config))))
              (error "BAD local-includes in emacs-project-config.json"))
          (if (and (not (null (custom/map/get "global-includes" config)))
                   (not (listp (custom/map/get "global-includes" config))))
              (error "BAD global-includes in emacs-project-config.json"))
          (if (and (not (null (custom/map/get "macro-table" config)))
                   (not (hash-table-p (custom/map/get "macro-table" config))))
              (error "BAD macro-table in emacs-project-config.json"))
          (if (hash-table-p (custom/map/get "macro-table" config))
              (custom/map/set "macro-table"
                              (custom/map/to-alist (custom/map/get "macro-table"
                                                                   config))
                              config))
          (if (and (not (null (custom/map/get "macro-files" config)))
                   (not (listp (custom/map/get "macro-files" config))))
              (error "BAD macro-files in emacs-project-config.json"))
          (if (and (not (null (custom/map/get "source-roots" config)))
                   (not (listp (custom/map/get "source-roots" config))))
              (error "BAD source-roots in emacs-project-config.json"))
          (let* ((macro-files (custom/map/get "macro-files"
                                              config))
                 (not-found-files (seq-remove 'file-exists-p
                                              macro-files)))
            (if (not (equal (length not-found-files) 0))
                (progn
                  (setq macro-files (seq-filter 'file-exists-p macro-files))
                  (if (and (hash-table-p config)
                           (yes-or-no-p (concat "Found nonexistent macro files "
                                                (format "%s" not-found-files)
                                                ". Do you want to remove them from JSON config?")))
                      (progn
                        (custom/map/set "macro-files" macro-files config)
                        (let ((json-encoding-pretty-print t)
                              (json-encoding-default-indentation "    ")
                              (json-encoding-object-sort-predicate 'string<))
                          (f-write-text (json-encode config)
                                        'utf-8
                                        config-path))))))))
      (if (not (equal config nil))
          (error "BAD FORMAT OF %s" config-path)
        (custom/ede/generate-config-file (file-name-as-directory proj-root))))
    config))

(defun custom/ede/load-project (name proj-root)
  (let* ((config (custom/ede/load-config-file proj-root)))
    (ede-cpp-root-project name
                          :file (concat (file-name-as-directory proj-root)
                                        ".dir-locals.el")
                          :include-path (delete-dups
                                         (append (custom/map/get
                                                  "local-includes"
                                                  config)
                                                 (list "/include" "../include")))
                          :system-include-path (delete-dups
                                                (custom/map/get
                                                 "global-includes"
                                                 config))
                          :spp-table (delete-dups (custom/map/get
                                                   "macro-table"
                                                   config))
                          :spp-files (delete-dups (custom/map/get
                                                   "macro-files"
                                                   config)))))

(defun custom/ede/generate-config-file (proj-root)
  (if (yes-or-no-p
       (format "Do you want to create default C/C++ config file at %s?"
               proj-root))
      (f-write-text (concat "{\n"
                            "\t\"local-includes\": [\"/include\",\"../include\"],\n"
                            "\t\"global-includes\": [],\n"
                            "\t\"source-roots\": [],\n"
                            "\t\"macro-table\": {},\n"
                            "\t\"macro-files\": []\n"
                            "}")
                    'utf-8
                    (concat proj-root
                            "emacs-project-config.json"))))

(defun custom/ede/generate-generic-loader (proj-root)
  (let ((proj-name (read-string "Project name: "
                                (let ((fragments (s-split custom/fs-separator
                                                          (file-name-as-directory proj-root))))
                                  (nth (- (length fragments) 2)
                                       fragments)))))
    `((custom/ede/load-project ,proj-name
                               ,(file-name-as-directory proj-root)))))

;;;; CODE NAVIGATION ROUTINES
;; CODE:

(defun custom/semantic-goto-definition (point)
  "Goto definition using semantic-ia-fast-jump
save the pointer marker if tag is found"
  (interactive "d")
  (condition-case err
      (progn
        (xref-push-marker-stack)
        (semantic-ia-fast-jump point)
        (if (not semantic-idle-scheduler-mode)
            (semantic-idle-scheduler-mode))
        (recenter))
    (error
     (xref-pop-marker-stack)
     (signal (car err) (cdr err)))))

(defun custom/semantic-switch-proto ()
  "Goto definition using semantic-ia-fast-jump
save the pointer marker if tag is found"
  (interactive)
  (condition-case err
      (progn
        (xref-push-marker-stack)
        (semantic-analyze-proto-impl-toggle)
        (if (not semantic-idle-scheduler-mode)
            (semantic-idle-scheduler-mode))
        (recenter))
    (error
     (xref-pop-marker-stack)
     (signal (car err) (cdr err)))))

(cl-defmethod semantic-symref-result-get-tags-as-is ((result semantic-symref-result)
                                                     &optional open-buffers)
  "Get the list of tags from the symref result RESULT.
Optional OPEN-BUFFERS indicates that the buffers that the hits are
in should remain open after scanning.
Note: This can be quite slow if most of the hits are not in buffers
already."
  (if (and (slot-boundp result 'hit-tags) (oref result hit-tags))
      (oref result hit-tags)
    ;; Calculate the tags.
    (let ((lines (oref result hit-lines))
          (txt (oref (oref result created-by) searchfor))
          (searchtype (oref (oref result created-by) searchtype))
          (ans nil)
          (out nil))
      (save-excursion
        (setq ans (mapcar
                   (lambda (hit)
                     (semantic-symref-hit-to-tag-via-buffer
                      hit txt searchtype open-buffers))
                   lines)))
      ;; Kill off dead buffers, unless we were requested to leave them open.
      (if (not open-buffers)
          (add-hook 'post-command-hook 'semantic-symref-cleanup-recent-buffers-fcn)
        ;; Else, just clear the saved buffers so they aren't deleted later.
        (setq semantic-symref-recently-opened-buffers nil)
        )
      ans)))

(defun custom/semantic/complete-jump (sym)
  (interactive (list (read-string "Look for symbol: "
                                  (thing-at-point 'symbol))))
  ;; Use modified symref module for getting all tags with target name in
  ;; current project and all its dependencies.
  ;; Bypasses bug when brute deep searching all tables in project
  ;; using standard semantic find routines.
  (let ((tags (let ((res (semantic-symref-find-tags-by-name sym)))
                (semantic-symref-result-get-tags-as-is res))))
    (if tags
        (let* ((chosen-tag (custom/semantic/choose-tag tags)))
          (custom/semantic/goto-tag chosen-tag))
      (message "No tags found for %s" sym))))

(defvar-local *is-prefix-pointer-state*
  (cons nil 'allow))
(defun custom/semantic/is-prefix-pointer-p (&optional point)
  (interactive "d")
  (if (not point)
      (setq point (point)))
  (when (not (custom/pos-is-in-comment point))
    (let* ((guess-if-pointer
            (lambda (sym)
              (if sym
                  (let ((case-fold-search nil))
                    (s-match (pcre-to-elisp/cached "^p[A-Z].*") sym)))))
           (ctx-sym (cadr (semantic-ctxt-current-symbol-and-bounds point)))
           (curr-fun-name (custom/semantic/get-current-function-name))
           (decision (not (and (equal curr-fun-name
                                      (car *is-prefix-pointer-state*))
                               (not (equal (cdr *is-prefix-pointer-state*)
                                           'allow)))))
           (ctx-with-time
            (custom/with-measure-time
                (if decision
                    (ignore-errors (semantic-analyze-current-context point)))))
           (ctx (car ctx-with-time))
           (run-time (cdr ctx-with-time))
           (prefix (if ctx (oref ctx prefix) nil))
           (last-pref (car (last prefix))))
      (when (> run-time 2)
        (setq *is-prefix-pointer-state*
              (cons curr-fun-name 'no-ctx-fetch))
        (when (yes-or-no-p (format
                            (concat "Context fetch was too slow (%s) so it was "
                                    "disabled temporary for this function (%s). Do "
                                    "you want to enable type guessing (pSomething "
                                    "is considered as a pointer)?")
                            run-time
                            curr-fun-name))
          (setq *is-prefix-pointer-state*
                (cons curr-fun-name 'guess))))
      (if last-pref
          (if (semantic-tag-p last-pref)
              (semantic-tag-get-attribute last-pref
                                          :pointer)
            (if (equal (cdr *is-prefix-pointer-state*)
                       'guess)
                (funcall guess-if-pointer ctx-sym)))
        (if (equal (cdr *is-prefix-pointer-state*)
                   'guess)
            (funcall guess-if-pointer ctx-sym))))))

;; SOME NOTES ABOUT SEMANTIC RESEARCH
;;
;; Functions for analyzation of context around point
;; It is used for autocompletion and for smart jumps
;; via semantic-ia-fast-jump
;;
;;     Get/Set data from/to overlay -> (semantic-analyze-current-context)
;;     Generate conteext -> (semantic-analyze-current-context-default)
;;     Context is generated            ||
;;     by parsing local syms           ||
;;     and linking them against        ||
;;     scope tags                      ||
;;                                     ||
;;                                     \/
;;     Current prefix tags: (oref (semantic-analyze-current-context) prefix)
;;     Enclosing tag: (oref (oref (semantic-analyze-current-context) scope)
;;                           tag)
;;     Scope tags: (oref (oref (semantic-analyze-current-context) scope)
;;                       scope)
;;     local var tags: (oref (oref (semantic-analyze-current-context) scope)
;;                           localvar)
;;                                     ...
;;     And many many more info, its gold mine for context type info
;;
;; Internally semantic-analyze-current-context-default uses
;; semantic-calculate-scope for scope calculation
;; by scope calculation, it is meant collection of all accessible tags
;; from curent location including local variables
;;
;;
;; Control tag sources when quering DB
;;   (semanticdb-find-default-throttle)
;; Add preprocesor value for all projects
;;   (semantic-c-add-preprocessor-symbol "__SYM__" "VAL")
;;
;; System includes are not project includes, these apply for
;; every project (and are not saved)
;;   (semantic-add-system-include include-root-dir symbol-for-mode)
;;   (semantic-remove-system-include include-root-dir symbol-for-mode)
;;
;; CODE:

;;;; TAGS ACCESS AND VARIOUS INDEX QUERIES
;; CODE:

;; with help of custom/eieo-inspect
(defun custom/semantic/get-local-tags (&optional path brutish live-tags)
  (interactive)
  (let ((tags (loop for tag in (loop for table
                                     in (semanticdb-find-translate-path path
                                                                        brutish)
                                     append (oref table tags))
                    for tagbuff = (semantic-tag-buffer tag)
                    collect (list (if live-tags tag (semantic-tag-copy tag nil t))
                                  (semantic-tag-name tag)
                                  (semantic-tag-class tag)
                                  (if tagbuff (buffer-name tagbuff) nil)
                                  (if tagbuff (buffer-file-name tagbuff) nil)
                                  tagbuff))))
    (if (called-interactively-p 'any)
        (if tags
            (custom/with-simple-pop-up "*LOCAL SYMS*"
              (dolist (tag tags)
                (let* ((cls (format "%s" (caddr tag)))
                       (cls-len (length cls))
                       (name (s-truncate 40 (cadr tag)))
                       (nice-proto (semantic-format-tag-prototype (car tag)
                                                                  nil
                                                                  t))
                       (first-gap (- 10 cls-len))
                       (second-gap (max 10
                                        (- 60
                                           (+ cls-len
                                              (length name)
                                              first-gap)))))
                  (insert (concat cls
                                  (s-repeat first-gap " ")
                                  name
                                  (s-repeat second-gap " ")
                                  nice-proto
                                  "\n"))))))
      tags)))

(defun custom/semantic/get-scope-tags (&optional point)
  (interactive "d")
  (let ((tags (loop for tag in (semantic-get-all-local-variables)
                    collect (list tag
                                  (semantic-tag-name tag)
                                  (semantic-tag-class tag)
                                  nil
                                  nil
                                  nil))))
    (if (called-interactively-p 'any)
        (if tags
            (custom/with-simple-pop-up "*FUNCTION VARS*"
              (dolist (tag tags)
                (insert (concat (format "%s" (caddr tag))
                                " "
                                (cadr tag)
                                (s-repeat (max 10
                                               (- 60 (+ 9
                                                        (length (cadr tag)))))
                                          " ")
                                (semantic-format-tag-prototype
                                 (car tag) nil t)
                                "\n"))))
          (message "Failed to find local variables."))
      tags)))

(defun custom/semantic/get-global-tags (sym &optional live-tags)
  (interactive (list (read-string "Enter tag name: ")))
  (let ((tags (loop for tag
                    in (semanticdb-strip-find-results
                        (semanticdb-brute-deep-find-tags-by-name sym nil t)
                        t)
                    for tagbuff = (semantic-tag-buffer tag)
                    if tagbuff
                    collect (list (if live-tags tag (semantic-tag-copy tag nil t))
                                  (semantic-tag-name tag)
                                  (semantic-tag-class tag)
                                  (buffer-name tagbuff)
                                  (buffer-file-name tagbuff)
                                  tagbuff))))
    (if (called-interactively-p 'any)
        (if tags
            (custom/with-simple-pop-up "*QUERY SYMS*"
              (dolist (tag tags)
                (let* ((cls (format "%s" (caddr tag)))
                       (cls-len (length cls))
                       (name (s-truncate 40 (cadr tag)))
                       (nice-proto (semantic-format-tag-prototype (car tag)
                                                                  nil
                                                                  t))
                       (first-gap (- 10 cls-len))
                       (second-gap (max 10
                                        (- 60
                                           (+ cls-len
                                              (length name)
                                              first-gap))))
                       (filename (car (last tag 2)))
                       (third-gap (max 10
                                       (- 60
                                          (+ cls-len
                                             (length name)
                                             (length nice-proto)
                                             first-gap
                                             second-gap)))))
                  (insert (concat cls
                                  (s-repeat first-gap " ")
                                  name
                                  (s-repeat second-gap " ")
                                  nice-proto
                                  (s-repeat third-gap " ")
                                  filename
                                  "\n"))))))
      tags)))

(defun custom/semantic/flatten-enum-tags-table (&optional table)
  (let* ((table (semantic-something-to-tag-table table))
         (lists (list table)))
    (mapc (lambda (tag)
            ;; Unroll all unnamed enums in typedefs.
            (when (and (equal (semantic-tag-class tag) 'type)
                       (equal (semantic-tag-type tag) "typedef"))
              (setq tag (semantic-tag-get-attribute tag :typedef)))
            (when (and (equal (semantic-tag-class tag) 'type)
                       (equal (semantic-tag-type tag) "enum"))
              (let ((components (semantic-tag-components tag)))
                (if (and components
                         ;; unpositioned tags can be hazardous to
                         ;; completion.  Do we need any type of tag
                         ;; here?  - EL
                         (semantic-tag-with-position-p (car components)))
                    (push (custom/semantic/flatten-enum-tags-table components)
                          lists)))))
          table)
    (apply 'append (nreverse lists))))

(defun custom/semantic/flatten-tags-table (&optional table)
  (let* ((lists (list table)))
    (mapc (lambda (tag)
            ;; Optimize out function args. We are not interested in
            ;; them when processing grep hits.
            (when (not (equal (semantic-tag-class tag) 'function))
              (let ((components (semantic-tag-components tag)))
                (if (and components
                         ;; unpositioned tags can be hazardous to
                         ;; completion.  Do we need any type of tag
                         ;; here?  - EL
                         (semantic-tag-with-position-p (car components)))
                    (push (custom/semantic/flatten-tags-table components)
                          lists)))))
          table)
    (apply 'append (nreverse lists))))

(defun custom/semantic/get-local-tags-no-includes (file symbol)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (let ((tags (custom/semantic/flatten-tags-table
                   (semantic-fetch-tags))))
        (loop for tag in tags
              if (equal symbol (semantic-tag-name tag))
              collect tag)))))

(defun custom/semantic/get-current-tag ()
  (semantic-current-tag))

(defun custom/semantic/get-current-function-tag ()
  (let ((tag (semantic-current-tag)))
    (if (or (not tag)
            (not (equal (semantic-tag-class tag)
                        'function)))
        nil
      tag)))

(defun custom/semantic/get-current-function-name ()
  (let ((tag (custom/semantic/get-current-function-tag)))
    (if tag
        (semantic-tag-name tag)
      nil)))

(defun custom/semantic/choose-tag (tags &optional no-error)
  (when (and (not no-error)
             (equal (length tags) 0))
    (error "No tags provided for selection!"))
  (setq tags (loop for tag in tags
                   collect (semantic-tag-copy tag nil t)))
  (when (not (equal (length tags) 0))
    (let* ((tag-summary-f
            (lambda (tag)
              (format "%s = %s"
                      (let* ((str
                              (semantic-format-tag-prototype tag nil t))
                             (real-len (length str))
                             (limit-len 87))
                        (if (> real-len limit-len)
                            (concat (substring str 0 limit-len)
                                    "...")
                          str))
                      (let* ((str (buffer-file-name
                                   (semantic-tag-buffer tag)))
                             (real-len (length str))
                             (limit-len 57))
                        (if (> real-len limit-len)
                            (concat "..."
                                    (substring str (- real-len
                                                      limit-len)
                                               real-len))
                          str)))))
           (get-tag-by-summary-f (lambda (summary summaries)
                                   (loop for (summary2 tag) in summaries
                                         if (equal summary2 summary)
                                         return tag)))
           (seen-summaries (custom/map/create))
           (summaries (loop for tag in tags
                            for name = (semantic-tag-name tag)
                            for summary = (funcall tag-summary-f tag)
                            unless (prog1
                                       (custom/map/get summary seen-summaries)
                                     (custom/map/set summary t seen-summaries))
                            collect (list summary tag)))
           (chosen-summary
            (if (equal (length summaries) 1)
                (caar summaries)
              (ido-completing-read "Choose tag: "
                                   (loop for summary in summaries
                                         collect (car summary)))))
           (chosen-tag (funcall get-tag-by-summary-f
                                chosen-summary
                                summaries)))
      chosen-tag)))

(defun custom/semantic/goto-tag (tag)
  (xref-push-marker-stack)
  (find-file (buffer-file-name (semantic-tag-buffer tag)))
  (semantic-go-to-tag tag)
  (if (not semantic-idle-scheduler-mode)
      (semantic-idle-scheduler-mode))
  (recenter) (pulse-momentary-highlight-region
              (semantic-tag-start tag)
              (semantic-tag-end tag)))

;;;; INDEX MANAGEMENT CODE
;; CODE:

(defun custom/semantic/index-directory (root &optional selection-regex)
  (interactive (list (ido-read-directory-name "Select directory: ")))
  (let ((root (file-name-as-directory (file-truename root)))
        (files (directory-files root t)))
    (setq files (delete (format "%s." root) files))
    (setq files (delete (format "%s.." root) files))
    (setq files (delete (format "%s.git" root) files))
    (setq files (delete (format "%s.hg" root) files))
    (semanticdb-save-all-db)
    (while files
      (setq file (pop files))
      (if (not (file-accessible-directory-p file))
          (progn
            (when (string-match-p
                   (if selection-regex
                       selection-regex
                     (pcre-to-elisp/cached
                      ".*\\.(?:c|cpp|h|hpp|cxx|hxx)$"))
                   file)
              (ignore-errors
                (semanticdb-file-table-object file))))
        (progn
          (semanticdb-save-all-db)
          (custom/semantic/index-directory file))))))

;;;; SYMREF HACKS
;; CODE:

(defun semantic-symref-produce-list-on-results (res str)
  "Produce a symref list mode buffer on the results RES."
  (when (not res) (error "No references found"))
  (semantic-symref-result-get-tags res t)
  (message "Gathering References...done")
  ;; Build a references buffer.
  (let ((buff (get-buffer-create (format "*Symref %s" str))))
    (custom/universal-push-mark)
    (switch-to-buffer buff)
    (set-buffer buff)
    (semantic-symref-results-mode)
    (set (make-local-variable 'semantic-symref-current-results) res)
    (semantic-symref-results-dump res)
    (goto-char (point-min))))

(defun semantic-symref-rb-goto-file (&optional button)
  (interactive)
  (let* ((tag (button-get button 'tag))
         (buff (semantic-tag-buffer tag)))
    (custom/universal-push-mark)
    (switch-to-buffer buff)
    (pulse-momentary-highlight-one-line (point))))

(defun semantic-symref-rb-goto-tag (&optional button)
  (interactive)
  (let* ((tag (button-get button 'tag))
         (buff (semantic-tag-buffer tag)))
    (custom/universal-push-mark)
    (switch-to-buffer buff)
    (semantic-go-to-tag tag)
    (recenter)
    (pulse-momentary-highlight-one-line (point))))

(defun semantic-symref-rb-goto-match (&optional button)
  "Go to the file specified in the symref results buffer.
BUTTON is the button that was clicked."
  (interactive)
  (let* ((tag (button-get button 'tag))
         (line (button-get button 'line))
         (buff (semantic-tag-buffer tag)))
    (custom/universal-push-mark)
    (switch-to-buffer buff)
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)
    (pulse-momentary-highlight-one-line (point))))

(let ((map semantic-symref-results-mode-map))
  (define-key map (kbd "<C-right>") 'forward-button)
  (define-key map (kbd "<C-left>") 'backward-button)
  (define-key map (kbd "RET") 'push-button)
  (define-key map (kbd "+")  'semantic-symref-list-toggle-showing)
  (define-key map (kbd "-")  'semantic-symref-list-toggle-showing)
  (define-key map (kbd "=")  'semantic-symref-list-toggle-showing)
  (define-key map (kbd "SPC") 'semantic-symref-list-toggle-showing)
  (define-key map (kbd "C-+") 'semantic-symref-list-expand-all)
  (define-key map (kbd "C--") 'semantic-symref-list-contract-all)
  (define-key map (kbd "C-r") 'semantic-symref-list-rename-open-hits)
  (define-key map (kbd "R") 'semantic-symref-list-rename-open-hits)
  (define-key map (kbd "q") 'custom/universal-pop-mark)
  (define-key map (kbd "C-q") 'custom/universal-pop-mark))

;; Machinery for renaming local variables is part of symref facility so
;; it ended here.
(define-key srecode-field-keymap (kbd "<C-return>") 'srecode-field-exit-ask)

;; Adds support for multi repository symref searches. Will affect grep
;; presearch backend too.
(defun custom/semantic-symref-grep-use-template-around (oldfn
                                                        rootdir
                                                        filepattern
                                                        flags
                                                        pattern)
  (let* ((root (projectile-project-root))
         (proj-config (if root (custom/ede/load-config-file root)))
         (source-roots (if proj-config
                           (custom/map/get "source-roots"
                                           proj-config))))
    (setq rootdir (s-join " " (cons rootdir source-roots))))
  (let ((res (funcall oldfn rootdir filepattern flags pattern)))
    ;; (message "Generated grep command: %s" res)
    res))

(advice-add #'semantic-symref-grep-use-template
            :around #'custom/semantic-symref-grep-use-template-around)

;;;; GREP VIRTUAL DATABASE HACKS
;; CODE:

(defun custom/filter-raw-grep-output (hits searchfor)
  ;; (message "HITS %s" hits)
  (let ((deep-search #'custom/semantic/get-local-tags-no-includes)
        (files (@dict-new))
        (result nil))
    (loop for (line . path) in hits
          do (@dict-set path t files))
    (@map (lambda (filename dummy)
            (setq result
                  (append result
                          (loop for tag
                                in (funcall deep-search
                                            filename
                                            searchfor)
                                collect (cons tag filename)))))
          files)
    (setq result
          (loop for (tag . file) in result
                collect (cons (with-current-buffer (find-file-noselect file)
                                (line-number-at-pos
                                 (semantic-tag-start tag)))
                              file)))
    ;; (message "RESULT %s" result)
    result))

(setq *custom/semantic/grep-db-enabled* t)
(cl-defmethod semantic-symref-perform-search ((tool semantic-symref-tool-grep))
  "Perform a search with Grep."
  (when *custom/semantic/grep-db-enabled*
    ;; Grep doesn't support some types of searches.
    (let ((st (oref tool searchtype)))
      (when (not (memq st '(symbol regexp tagname)))
        (error "Symref impl GREP does not support searchtype of '%s' for '%s'!"
               st
               (oref tool searchfor))))
    ;; Find the root of the project, and do a find-grep...
    (let* (;; Find the file patterns to use.
           (rootdir (semantic-symref-calculate-rootdir))
           (filepatterns (semantic-symref-derive-find-filepatterns))
           (filepattern (mapconcat #'shell-quote-argument filepatterns " "))
           ;; Grep based flags.
           (grepflags (cond ((eq (oref tool resulttype) 'file)
                             "-l ")
                            ((eq (oref tool searchtype) 'regexp)
                             "-nE ")
                            ((eq (oref tool searchtype) 'symbol)
                             (if (> emacs-major-version 25)
                                 ;; Evil hack for workarounding strange
                                 ;; grep issue in older emacs.
                                 "-n "
                               "-nw "))
                            ((eq (oref tool searchtype) 'tagname)
                             ;; Evil hack for workarounding strange
                             ;; grep issue in older emacs.
                             (if (> emacs-major-version 25)
                                 "-n "
                               "-nw "))
                            (t "-n ")))
           (greppat (cond ((eq (oref tool searchtype) 'regexp)
                           (oref tool searchfor))
                          (t
                           ;; Can't use the word boundaries: Grep
                           ;; doesn't always agree with the language
                           ;; syntax on those.
                           ;;
                           ;; Some error in grep command
                           ;; preprocessing somewhere.
                           (if (> emacs-major-version 25)
                               (format "\\(^\\|\\W\\)%s\\(\\W\\|$\\)"
                                       (oref tool searchfor))
                             (oref tool searchfor)))))
           ;; Misc
           (b (get-buffer-create "*Semantic SymRef*"))
           (ans nil))
      (with-current-buffer b
        (erase-buffer)
        (setq default-directory rootdir)
        (let ((cmd (semantic-symref-grep-use-template rootdir
                                                      filepattern
                                                      grepflags
                                                      greppat)))
          (call-process semantic-symref-grep-shell nil b nil
                        shell-command-switch cmd)))
      (setq ans (semantic-symref-parse-tool-output tool b))
      ;; Special handling for search type tagname.
      ;; Semantic expect only positions of tags will be provided so we
      ;; must filter raw output from grep using semantic parsing
      ;; infrastructure.
      (if (equal (oref tool searchtype) 'tagname)
          (custom/filter-raw-grep-output ans
                                         (oref tool searchfor))
        ans))))

;; Disable grepping in idle summary mode. We need just prototype here,
;; no need to scan whole project by grep.
(advice-add 'semantic-idle-summary-idle-function :around
            (lambda (oldfn)
              (let ((*custom/semantic/grep-db-enabled* nil))
                (funcall oldfn))))

;;;; CODE DOC ACQUISITION
;; CODE:

(defun semantic-ia-show-doc (point)
  "Display the code-level documentation for the symbol at POINT."
  (interactive "d")
  (let* ((actual-buffer (current-buffer))
         (ctxt (semantic-analyze-current-context point))
         (pf (reverse (oref ctxt prefix)))
         (tag (car pf)))
    ;; If PF, the prefix is non-nil, then the last element is either
    ;; a string (incomplete type), or a semantic TAG.  If it is a TAG
    ;; then we should be able to find DOC for it.
    (cond
     ((stringp tag)
      (message "Incomplete symbol name."))
     ((semantic-tag-p tag)
      ;; Here we are calling semantic-analyze-tag-references in order
      ;; to find either prototype or definition of current tag because
      ;; in some projects, main documentation reside above prototype
      ;; and in other, above implementation so we need to acquire
      ;; documentation for both and use the one which is more
      ;; complete. Currently indication of completeness is string
      ;; length which is probably not optimal but good enough for now.
      (let* ((sar (semantic-analyze-tag-references tag))
             (other-tag (if (semantic-tag-prototype-p tag)
                            (car (semantic-analyze-refs-impl sar t))
                          (car (semantic-analyze-refs-proto sar t))))
             (doc (or (semantic-documentation-for-tag tag) ""))
             (other-doc (or (if other-tag
                                (semantic-documentation-for-tag other-tag))
                            ""))
             (args-list-fat
              (loop for arg in (semantic-tag-function-arguments tag)
                    if (not (equal (semantic-tag-name arg) ""))
                    collect (let ((formatted-arg
                                   (semantic-format-tag-prototype arg
                                                                  nil
                                                                  t)))
                              (list (pcre-to-elisp/cached
                                     (concat "(\\W)"
                                             (semantic-tag-name arg)
                                             "(\\W)"))
                                    (concat "\\1("
                                            formatted-arg
                                            ")\\2")
                                    (custom/semantic/find-type-tags-of-var
                                     arg
                                     actual-buffer)))))
             (args-list (loop for entry in args-list-fat
                              collect (cons (@at entry 0)
                                            (@at entry 1)))))
        (if (and (string= doc "") (string= other-doc ""))
            (message "Doc unavailable!")
          (custom/with-simple-pop-up "*TAG DOCUMENTATION*"
            (setq kill-on-quit t)
            (insert (semantic-format-tag-prototype tag nil t))
            (insert "\n")
            (insert "\n")
            (insert (if (> (length doc) (length other-doc))
                        (custom/replace-all args-list doc)
                      (custom/replace-all args-list other-doc)))
            (loop for entry in args-list-fat
                  do (custom/attach-buttons-to-label-occurences
                      (substring (@at entry 1) 2 -2)
                      (@at entry 2)))))))
     (t
      (message "Unknown tag.")))))

(defun custom/semantic/find-type-tags-of-var (tag &optional buffer)
  (if (not buffer)
      (setq buffer (current-buffer)))
  (let ((*custom/semantic/grep-db-enabled* nil))
    (semanticdb-strip-find-results
     (semanticdb-deep-find-tags-by-name
      (if (equal (semantic-tag-class tag) 'variable)
          (let ((tag-type (semantic-tag-type tag)))
            (if (stringp tag-type)
                tag-type
              (semantic-tag-name tag-type)))
        (error "Found invalid tag '%s' with name '%s'!"
               (semantic-tag-class tag)
               (semantic-tag-name tag)))
      buffer t)
     t)))

(defun custom/attach-buttons-to-label-occurences (string tags)
  (loop for (beg . end) in (custom/find-string-occurences string)
        do (progn (make-button beg end
                               'mouse-face 'custom-button-pressed-face
                               'face nil
                               'action 'custom/doc-button-handler
                               'tags tags)
                  (add-face-text-property beg end 'underline)
                  (add-face-text-property beg end 'bold))))

(defun custom/doc-button-handler (&optional button)
  (interactive)
  (let* ((tags (button-get button 'tags))
         (selected-tag
          (if (equal (length tags) 1)
              (car tags)
            (custom/semantic/choose-tag tags))))
    (custom/semantic/goto-tag selected-tag)))

(define-overloadable-function semantic-documentation-for-tag (&optional tag nosnarf)
  "Find documentation from TAG and return it as a clean string.
TAG might have DOCUMENTATION set in it already.  If not, there may be
some documentation in a comment preceding TAG's definition which we
can look for.  When appropriate, this can be overridden by a language specific
enhancement.
Optional argument NOSNARF means to only return the lexical analyzer token for it.
If NOSNARF is `lex', then only return the lex token."
  (if (not tag)
      (setq tag (semantic-current-tag)))
  (when (semantic-tag-buffer tag)
    (with-current-buffer (semantic-tag-buffer tag)
      (:override
       ;; No override.  Try something simple to find documentation nearby
       (save-excursion
         (semantic-go-to-tag tag)
         (let ((doctmp (semantic-tag-docstring tag (current-buffer))))
           (or doctmp
               (when (semantic-tag-with-position-p tag)
                 (semantic-documentation-comment-preceding-tag tag nosnarf))
               nil)))))))

(defun semantic-documentation-comment-preceding-tag (&optional tag nosnarf)
  (if (not tag)
      (setq tag (semantic-current-tag)))
  (save-excursion
    (semantic-go-to-tag tag)
    (let* ((tag-start (semantic-tag-start tag))
           (starttag (semantic-find-tag-by-overlay-prev tag-start))
           (start (if starttag
                      (semantic-tag-end starttag)
                    (point-min)))
           (stop (semantic-tag-start tag))
           (raw-comment (custom/extract-comments-from-region start stop)))
      ;; Comment cleanup. Removes comment symbols and preprocess doxygen.
      (custom/chain-forms
       (s-replace-regexp (pcre-to-elisp/cached
                          "(?:///+[ \\t]*|//-+[ \\t]*)")
                         "" raw-comment)
       (s-replace-regexp (pcre-to-elisp/cached
                          "\\A\\s*|\\s*\\Z")
                         "")
       (s-replace-regexp (pcre-to-elisp/cached
                          "^[ \t]*")
                         "")
       (s-replace-regexp (pcre-to-elisp/cached
                          "/\\*[ \t]*")
                         "")
       (s-replace-regexp (pcre-to-elisp/cached
                          "[ \t]*\\*/")
                         "")
       (s-replace-regexp (pcre-to-elisp/cached
                          "@brief[ \t]*")
                         "")
       (s-replace-regexp (pcre-to-elisp/cached
                          "@return[ \t]*")
                         "=== return ===\n")
       (s-replace-regexp (pcre-to-elisp/cached
                          "@retval[ \t]*(\\w+)[ \t]*")
                         "Option \\1 = ")
       (s-replace-regexp (pcre-to-elisp/cached
                          "@param\\[([^\\]]*)\\][ \t]*(\\w+)[ \t]*")
                         "Parameter[\\1] \\2 = ")
       (s-replace-regexp (pcre-to-elisp/cached
                          "^//\\s*")
                         "")
       (s-replace-regexp (pcre-to-elisp/cached
                          "^\\*+$")
                         "")
       (s-replace-regexp (pcre-to-elisp/cached
                          "^\\*\\s*(.+)\\s*\\*$")
                         "\\1")))))


;; TODO: Implement simple semantic aware font locking.

;; Extend semantic-default-c-setup to also check if buffer is in
;; projectile project. If yes, try to load config file and register
;; project into EDE according to data inside it.
;; Or create my own EDE project class which will be using directly
;; JSON config.
;;
;; Another thing is that I can populate macro definition list here
;; with some constants used by std lib and potentially QT. It causes
;; parse errors without it.
;;
;; ede-cpp-root-project-list is a list of all registered projects
;; oref and with-slots for object inspection
;;
;; Use this for detection if project was registered and for dynamic
;; update of project info from config file.
;; Update project if save operation occurred in something what looks
;; like project config file.
;; Also advice revert-buffer and perform this on all reverted files
;; which looks like project config files.

;; TODO: Try to download and compile newest parser definition from
;; https://sourceforge.net/p/cedet/git/ci/master/tree/lisp/cedet/semantic/bovine/c.by
;; It might fix some things and provide speedup.

;; NOTE: Interesting functions where include resolve process occurs
;;   semantic-dependency-tag-file
;;   semantic-dependency-find-file-on-path

;; NOTE semanticdb-find-translate-path-brutish-default
;; Dont return tables of project dependencies.
