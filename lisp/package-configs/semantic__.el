;; WARNING after emacs update (together with CEDET)
;; you must delete whole semnatic cache becuase it is in invalid
;; format (better said, incompatible with new version) and semantic
;; simply silently fails instead of detecting incompatibility

(require 'ede)
(require 'cc-mode)
(require 'semantic)
(require 'cedet-global)
(require 'cedet-cscope)
(require 'semantic/idle)
(require 'semantic/db-ebrowse)
(require 'srecode)
(require 'json)
(require 'seq)
(if (not (featurep 'semantic/db-cscope))
    (load "external-semantic-db-cscope.el")
  (require 'semantic/db-cscope))

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

(setq cedet-global-command "global")
(if (cedet-gnu-global-version-check t)
   (progn
     (semanticdb-enable-gnu-global-databases 'c-mode)
     (semanticdb-enable-gnu-global-databases 'c++-mode)))

(setq cedet-cscope-command "cscope")
(if (cedet-cscope-version-check t)
    (if (functionp 'semanticdb-enable-cscope-databases)
        (semanticdb-enable-cscope-databases)))

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
                           (semantic-force-refresh))
                          (if (or (equal major-mode 'c-mode)
                                  (equal major-mode 'c++-mode))
                              (custom/ede/create-update-index
                               (projectile-project-root))))))))
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
                         (semantic-force-refresh))
                        (if (or (equal major-mode 'c-mode)
                                (equal major-mode 'c++-mode))
                            (custom/ede/create-update-index
                             (projectile-project-root)))))))))

(global-ede-mode 1)
(semantic-mode 1)



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
                                                   config)))
    (custom/ede/create-update-index proj-root
                                    (custom/map/get "source-roots" config))))

(defvar *should-semantic-parse-all* nil)
(defun custom/ede/create-update-index (root &optional dependecies)
  (interactive (projectile-project-root))
  (if (equal dependecies nil)
      (setq dependecies
            (custom/map/get "source-roots"
                            (custom/ede/load-config-file root))))
  (cond
   ((cedet-gnu-global-version-check t)
    (setq dependecies (mapcar #'file-truename dependecies))
    (dolist (dep dependecies)
      (cedet-gnu-global-create/update-database dep))
    (setenv "GTAGSLIBPATH" (string-join dependecies ":"))
    (cedet-gnu-global-create/update-database root)
    (semantic-symref-detect-symref-tool))
   ((and (cedet-cscope-version-check t)
         (functionp 'semanticdb-enable-cscope-databases))
    (custom/make-link-farm (concat (file-name-as-directory root)
                                   "emacs-project-src-roots")
                           dependecies)
    (cedet-cscope-create/update-database root)
    (semantic-symref-detect-symref-tool))
   (t
    (if (equal *should-semantic-parse-all* nil)
        (setq *should-semantic-parse-all*
              (if (yes-or-no-p
                   (concat "No GNU Global nor CScope found, do you want to "
                           "prefetch all symbols from all source roots "
                           "in this project? (may be time and memory "
                           "consuming for big project [only for first time])"))
                  "yes"
                "no")))

    (if (equal *should-semantic-parse-all* "yes")
        (dolist (r (cons root (delete-dups dependecies)))
          (custom/semantic/index-from-root r))))))

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

(defun custom/ede/refresh-global-deps (root)
  (let* ((config (custom/ede/load-config-file root))
         (deps (if config
                   (custom/map/get "source-roots" config)
                 nil)))
    (if deps
        (progn
          (setq deps (mapcar #'file-truename deps))
          (setenv "GTAGSLIBPATH" (string-join deps ":"))))))

(defun custom/ede/generate-generic-loader (proj-root)
  (let ((proj-name (read-string "Project name: "
                                (let ((fragments (s-split custom/fs-separator
                                                          (file-name-as-directory proj-root))))
                                  (nth (- (length fragments) 2)
                                       fragments)))))
    `((custom/ede/load-project ,proj-name
                               ,(file-name-as-directory proj-root)))))

(defun custom/ede/generate-generic-refresher (proj-root)
  `((custom/ede/refresh-global-deps
     ,(file-name-as-directory proj-root))))

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
          (let* ((tag-summary-f (lambda (tag)
                                  (format "%s IN FILE %s"
                                          (semantic-format-tag-prototype tag nil t)
                                          (buffer-file-name (semantic-tag-buffer tag)))))
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

(defun custom/semantic-pop-tag-mark ()
  "popup the tag save by semantic-goto-definition"
  (interactive)
  (xref-pop-marker-stack)
  (recenter)
  (pulse-momentary-highlight-one-line (point)))

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
;; Control tag sources wen quering DB
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
