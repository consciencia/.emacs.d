;; WARNING after emacs update (together with CEDET)
;; you must delete whole semnatic cache becuase it is in invalid
;; format (better said, incompatible with new version) and semantic
;; simply silently fails instead of detecting incompatibility

(require 'ede)
(require 'ede/cpp-root)
(require 'cc-mode)
(require 'semantic)
(require 'cedet-global)
(require 'cedet-cscope)
(require 'semantic/idle)
(require 'semantic/db-ebrowse)
(require 'semantic/symref)
(require 'semantic/symref/grep)
(require 'semantic/symref/list)
(require 'srecode)
(require 'json)
(require 'seq)
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

(if (functionp 'semantic-default-elisp-setup)
    (progn
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
                        (progn
                          (save-mark-and-excursion
                            (semantic-force-refresh)))))))
  (progn
    (add-to-list 'semantic-inhibit-functions
                 (lambda ()
                   (not (or (equal major-mode 'c-mode)
                            (equal major-mode 'c++-mode)))))
    (advice-add 'save-buffer :after
                (lambda (&rest args)
                  (if (or (equal major-mode 'c-mode)
                          (equal major-mode 'c++-mode))
                      (progn
                        (save-mark-and-excursion
                          (semantic-force-refresh))))))))

(global-ede-mode 1)
(semantic-mode 1)



(defun semantic-symref-cleanup-recent-buffers-fcn ()
  ;; This function needs to be stubbed in order to prevent deletion
  ;; of lots of files loaded during symref searching. Whole load
  ;; process is very long, so its good idea to just cache files.
  )

;; Hack used for company complete c headers.
(cl-defmethod ede-include-path ((this ede-cpp-root-project))
  "Get the system include path used by project THIS."
  (oref this include-path))

(cl-defmethod ede-include-path ((this ede-cpp-root-target))
  "Get the system include path used by target THIS."
  (ede-include-path (ede-target-parent this)))

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

(defun custom/semantic/complete-jump (sym)
  (interactive (list
                (read-string "Look for symbol: "
                             (thing-at-point 'symbol))))
  (let ((tags (custom/semantic/query-all-tags-in-proj sym)))
    (if tags
        (progn
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
                 (summaries (loop for (tag name class filename filepath buff)
                                  in tags
                                  for summary = (funcall tag-summary-f tag)
                                  if (equal name sym)
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
            (if chosen-tag
                (progn
                  (push-mark)
                  (xref-push-marker-stack)
                  (find-file (buffer-file-name (semantic-tag-buffer chosen-tag)))
                  (goto-char (semantic-tag-start chosen-tag))
                  (if (not semantic-idle-scheduler-mode)
                      (semantic-idle-scheduler-mode))
                  (recenter)
                  (pulse-momentary-highlight-region (semantic-tag-start chosen-tag)
                                                    (semantic-tag-end chosen-tag)))
              (message "Error, failed to pair tags and summaries -> REPORT BUG"))))
      (message "No tags found for %s" sym))))

(defvar-local *is-prefix-pointer-state*
  (cons nil 'allow))
(defun custom/semantic/is-prefix-pointer-p (&optional point)
  (interactive "d")
  (if (not point)
      (setq point (point)))
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
    (when (> run-time 1)
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
          (funcall guess-if-pointer ctx-sym)))))

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
;; Symstem includes are not project includes, these apply for
;; every project (and are not saved)
;;   (semantic-add-system-include include-root-dir symbol-for-mode)
;;   (semantic-remove-system-include include-root-dir symbol-for-mode)
;;
;; CODE:

;;;; TAGS ACCESS AND VARIOUS INDEX QUERIES
;; CODE:

;; with help of custom/eieo-inspect
(defun custom/semantic/get-local-all-tags (&optional path brutish live-tags)
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

(defun custom/semantic/get-function-var-tags (&optional point)
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

(defun custom/semantic/query-all-tags-in-proj (sym &optional live-tags)
  (interactive (list (read-string "Enter tag name: ")))
  (let ((tags (loop for tag
                    in (semanticdb-strip-find-results
                        (semanticdb-brute-deep-find-tags-by-name sym nil nil)
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

;;;; INDEX MANAGEMENT CODE
;; CODE:

(defun custom/semantic/index-from-root-raw (root &optional selection-regex)
  (let ((root (file-name-as-directory (file-truename root)))
        (files (directory-files root t)))
    (setq files (delete (format "%s." root) files))
    (setq files (delete (format "%s.." root) files))
    (setq files (delete (format "%s.git" root) files))
    (setq files (delete (format "%s.hg" root) files))
    (while files
      (setq file (pop files))
      (if (not (file-accessible-directory-p file))
          (progn
            (when (string-match-p (if selection-regex
                                      selection-regex
                                    ".*\\.\\(c\\|cpp\\)$")
                                  file)
              (ignore-errors
                (semanticdb-file-table-object file))))
        (progn
          (semanticdb-save-all-db)
          (custom/semantic/index-from-root-raw file))))))

(defun custom/semantic/index-from-root (root &optional selection-regex)
  (interactive (list (ido-read-directory-name "Write index root: ")))
  (custom/semantic/index-from-root-raw root
                                       selection-regex)
  (semanticdb-save-all-db))

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
    (pulse-momentary-highlight-one-line (point))))

(let ((map semantic-symref-results-mode-map))
  (define-key map (kbd "<C-right>") 'forward-button)
  (define-key map (kbd "<C-left>") 'backward-button)
  (define-key map (kbd "SPC") 'push-button)
  (define-key map (kbd "RET") 'push-button)
  (define-key map "+" 'semantic-symref-list-toggle-showing)
  (define-key map "-" 'semantic-symref-list-toggle-showing)
  (define-key map "=" nil)
  (define-key map (kbd "C-+") 'semantic-symref-list-expand-all)
  (define-key map (kbd "C--") 'semantic-symref-list-contract-all)
  (define-key map (kbd "C-r") 'semantic-symref-list-rename-open-hits)
  (define-key map (kbd "R") 'semantic-symref-list-rename-open-hits)
  (define-key map (kbd "q") 'custom/universal-pop-mark)
  (define-key map (kbd "C-q") 'custom/universal-pop-mark))

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

(defun custom/is-tagname-hit (path line)
  (let ((buff (find-file-noselect path)))
    (with-current-buffer buff
      (goto-line line)
      (let* ((tag (semantic-current-tag))
             (tag-start (if tag (semantic-tag-start tag)))
             (tag-start-line (if tag-start (line-number-at-pos tag-start))))
        (equal tag-start-line line)))))

(cl-defmethod semantic-symref-perform-search ((tool semantic-symref-tool-grep))
  "Perform a search with Grep."
  (message "TYPE %s FOR %s"
           (oref tool searchtype)
           (oref tool searchfor))
  ;; TODO:
  ;;   tagcompletions
  ;;     treat that as type regexp and at post processing, treat it as tagname

  ;; Grep doesn't support some types of searches.
  (let ((st (oref tool searchtype)))
    (when (not (memq st '(symbol regexp tagname)))
      (error "Symref impl GREP does not support searchtype of %s"
             st)))
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
                          (t "-n ")))
         (greppat (cond ((eq (oref tool searchtype) 'regexp)
                         (oref tool searchfor))
                        (t
                         ;; Can't use the word boundaries: Grep
                         ;; doesn't always agree with the language
                         ;; syntax on those.
                         (format "\\(^\\|\\W\\)%s\\(\\W\\|$\\)"
                                 (oref tool searchfor)))))
         ;; Misc
         (b (get-buffer-create "*Semantic SymRef*"))
         (ans nil))
    (with-current-buffer b
      (erase-buffer)
      (setq default-directory rootdir)
      (if (not (fboundp 'grep-compute-defaults))
          ;; find . -type f -print0 | xargs -0 -e grep -nH -e
          ;; Note : I removed -e as it is not posix, nor necessary it seems.
          (let ((cmd (concat "find " default-directory " -type f " filepattern " -print0 "
                             "| xargs -0 grep -H " grepflags "-e " greppat)))
            ;;(message "Old command: %s" cmd)
            (call-process semantic-symref-grep-shell nil b nil
                          shell-command-switch cmd))
        (let ((cmd (semantic-symref-grep-use-template rootdir filepattern grepflags greppat)))
          (call-process semantic-symref-grep-shell nil b nil
                        shell-command-switch cmd))))
    (setq ans (semantic-symref-parse-tool-output tool b))
    ;; Special handling for search type tagname
    ;; Semantic expect only positions of tags will be provided so we
    ;; must filter raw output from grep using semantic parsing
    ;; infrastructure.
    (if (equal (oref tool searchtype) 'tagname)
        (loop for (line . path) in ans
              if (custom/is-tagname-hit path line)
              collect (cons line path))
      ans)))
