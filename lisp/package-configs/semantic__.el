;; WARNING after emacs update (together with CEDET)
;; you must delete whole semnatic cache becuase it is in invalid
;; format (better said, incompatible with new version) and semantic
;; simply silently fails instead of detecting incompatibility

;; may be handy
;;
;; semanticdb-find-default-throttle
;; (semantic-c-add-preprocessor-symbol "__SYM__" "VAL")
;; (semantic-add-system-include include-root-dir symbol-for-mode)

(require 'cc-mode)
(require 'semantic)
(require 'ede)
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
(global-semantic-stickyfunc-mode -1)
(global-semantic-highlight-edits-mode t)
(global-semantic-show-unmatched-syntax-mode -1)
(global-semantic-idle-summary-mode t)
(global-semantic-highlight-func-mode t)
(global-semantic-decoration-mode t)
(global-semantic-idle-breadcrumbs-mode t)

(setq-default semantic-idle-breadcrumbs-format-tag-function 'semantic-format-tag-summarize
              semantic-idle-work-parse-neighboring-files-flag t
              semantic-idle-work-update-headers-flag t
              semantic-complete-inline-analyzer-displayor-class 'semantic-displayor-tooltip
              semantic-edits-verbose-flag t
              speedbar-use-images nil
              speedbar-use-imenu-flag t
              senator-step-at-tag-classes nil
              senator-step-at-start-end-tag-classes (list 'function))

(setq cedet-global-command "global")
(if (cedet-gnu-global-version-check t)
   (progn
     (semanticdb-enable-gnu-global-databases 'c-mode)
     (semanticdb-enable-gnu-global-databases 'c++-mode)))

(setq cedet-cscope-command "cscope")
(if (cedet-cscope-version-check t)
    (if (functionp 'semanticdb-enable-cscope-databases)
        (semanticdb-enable-cscope-databases)))

(if (functionp 'semantic-default-elisp-setup)
    (progn
      (add-to-list 'semantic-new-buffer-setup-functions
                   '(emacs-lisp-mode . semantic-default-elisp-setup))
      (add-to-list 'semantic-inhibit-functions
                   (lambda ()
                     (not (or (equal major-mode 'c-mode)
                              (equal major-mode 'c++-mode)
                              (equal major-mode 'python-mode)
                              (equal major-mode 'emacs-lisp-mode)))))
      (advice-add 'save-buffer :after
                  (lambda (&rest args)
                    (if (or (equal major-mode 'c-mode)
                            (equal major-mode 'c++-mode)
                            (equal major-mode 'python-mode)
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
                            (equal major-mode 'python-mode)
                            (equal major-mode 'c++-mode)))))
    (advice-add 'save-buffer :after
                (lambda (&rest args)
                  (if (or (equal major-mode 'c-mode)
                          (equal major-mode 'python-mode)
                          (equal major-mode 'c++-mode))
                      (progn
                        (save-mark-and-excursion
                         (semantic-force-refresh))
                        (if (or (equal major-mode 'c-mode)
                                (equal major-mode 'c++-mode))
                            (custom/ede/create-update-index
                             (projectile-project-root)))))))))

(semantic-mode 1)
(global-ede-mode 1)



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
                   (concat "No GNU Global nor CTags found, do you want to "
                           "prefetch all symbols from all source roots "
                           "in this project? (may be time and memory "
                           "consuming for big project [only for first time])"))
                  "yes"
                "no")))

    (if (equal *should-semantic-parse-all* "yes")
        (dolist (r (delete-dups dependecies))
          (custom/semantic-index-specific r))))))

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

(defun custom/semantic-pop-tag-mark ()
  "popup the tag save by semantic-goto-definition"
  (interactive)
  (xref-pop-marker-stack)
  (pulse-momentary-highlight-one-line (point)))

(defun custom/semantic/complete-jump (sym)
  (interactive (list
                (read-string "Look for symbol: "
                             (thing-at-point 'symbol))))
  (let ((tags (custom/semantic/deep-brute-tags-query sym)))
    (if tags
        (progn
          (let* ((summaries (delete-dups
                             (mapcar #'custom/semantic/tag-summary
                                     (remove-if-not (lambda (val)
                                                      (equal (semantic-tag-name val)
                                                             sym))
                                                    tags))))
                 (chosen-summary (ido-completing-read "Choose tag: "
                                                      summaries))
                 (chosen-tag (custom/semantic/get-tag-by-summary chosen-summary
                                                                 tags)))
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

(defun custom/semantic/tag-summary (tag)
  (format "%s:%s -> %s"
          (semantic-tag-class tag)
          (semantic-tag-name tag)
          (buffer-file-name (semantic-tag-buffer tag))))

(defun custom/semantic/get-tag-by-summary (summary tags)
  (let ((res nil))
    (dolist (tag tags)
      (if (and (not res)
               (string= summary
                        (custom/semantic/tag-summary tag)))
          (setq res tag)))
    res))

(defun custom/semantic/deep-brute-tags-query (sym &optional live-tags buff)
"Query CEDET Semantic system for tag descriptors with sended name.
SYM: Name of symbol to find all occurences in DB.

OPTIONAL LIVE-TAGS: When t, tags are linked into Semantic machinery, that means, they
are updated when some buffer changes. On the other hand, dead tags are unlinked
from Semantic machinery and thus they are never changed even when some buffer contents
changes. Dead tags are usually the thing which you want.

BUFF: Buffer from which search should originate. Used for detection of project
and thus the retrival of all project tables used for deep search. Usually,
you want to search through current project so current buffer is automatically
provided"
  (let ((acc nil))
    (dolist (tag (semanticdb-strip-find-results
                  (semanticdb-brute-deep-find-tags-by-name
                   sym
                   (if buff
                       buff
                     (current-buffer)))
                  t))
      (if (semantic-tag-buffer tag)
          (setq acc (push tag acc))))
    (if live-tags
        acc
      (mapcar (lambda (tag)
                (semantic-tag-copy tag nil t))
              acc))))

(defun custom/semantic/get-enclosing-function-tag ()
  (semantic-fetch-tags)
  (let ((tag (semantic-current-tag)))
    (if (or (not tag)
            (not (equal (semantic-tag-class tag) 'function)))
        nil
      tag)))

(defun custom/c-api/get-enclosing-function-name ()
  (if semantic-mode
      (let ((tag (custom/semantic/get-enclosing-function-tag)))
        (if tag
            (semantic-tag-name tag)
          nil))
    (c-defun-name)))

(defun custom/semantic-index-dir-recur (root &optional selection-regex)
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
        ;;else if it's a directory
        (progn
          (semanticdb-save-all-db)
          (custom/semantic-index-dir-recur file))))))

(defun custom/semantic-index-specific (root)
  (interactive)
  (custom/semantic-index-dir-recur root)
  (semanticdb-save-all-db))

(defun custom/semantic-index-this-dir ()
  (interactive)
  (custom/semantic-index-dir-recur default-directory)
  (semanticdb-save-all-db))

(defun custom/semantic-index-this-projectile-project ()
  (interactive)
  (custom/semantic-index-dir-recur (projectile-project-root))
  (semanticdb-save-all-db))

(defun custom/semantic/full-reparse ()
  (interactive)
  (bovinate t))

(defun custom/semantic/debug-ia-fast-jump (point)
  (interactive "d")
  (let* ((ctxt (semantic-analyze-current-context point))
         (pf (and ctxt (reverse (oref ctxt prefix))))
         (first (car pf)))
    (if (semantic-tag-p first)
        (progn
          (setq PRIMARY-DEBUG first)
          (setq SECONDARY-DEBUG nil)
          (when (semantic-tag-prototype-p first)
            (let* ((refs (semantic-analyze-tag-references first))
                   (impl (semantic-analyze-refs-impl refs t)))
              (when impl
                (setq SECONDARY-DEBUG (car impl))))))
      (error "No context!"))))
