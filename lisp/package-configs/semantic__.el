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
(require 'semantic/ctxt)
(require 'semantic/bovine/c)
(require 'semantic/analyze)
(require 'mode-local)
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
;; Disabled because decoration mode awfully slows down parsing of
;; larger chunks of unparsed files.
;; Also, when turning on, it will iterate all buffers and turns itself on
;; in every buffer managed by semantic which is very slow when many
;; buffers are opened.
;; Because of that, its not good idea to switch it off and on when
;; parsing.
;; We will solve it by idle timer hack.
(global-semantic-decoration-mode -1)
(global-semantic-idle-breadcrumbs-mode t)
;; idle breadcrumbs are better, but are in conflict
;; with stickyfunc, so its disabled.
(global-semantic-stickyfunc-mode -1)
;; We dont want this globally, it kinds of annoys in some situations.
(global-semantic-show-unmatched-syntax-mode -1)

;; Active decoration mode for current buffer only after some seconds
;; of idling. This way, no slow down due to decoration happen during
;; bulk file parsing in grep and indexer.
(run-with-idle-timer 2 t 'custom/semantic/enable-decorations-locally)



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

;; Support for elisp slowes things down and I dont have time to find
;; out why. Another thing is that output from semantic is used only
;; for imenu so its not critical.
;;
;; (add-to-list 'semantic-new-buffer-setup-functions
;;              '(emacs-lisp-mode . semantic-default-elisp-setup))

(add-to-list 'semantic-inhibit-functions
             (lambda ()
               (not (or (equal major-mode 'c-mode)
                        (equal major-mode 'c++-mode)
                        ;; (equal major-mode 'emacs-lisp-mode)
                        ))))

(advice-add 'save-buffer :after
            (lambda (&rest args)
              (if (or (equal major-mode 'c-mode)
                      (equal major-mode 'c++-mode)
                      ;; (equal major-mode 'emacs-lisp-mode)
                      )
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
;; semanticdb-strip-find-results which caused company to lag for large
;; number of symbols.
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
              (custom/semantic/with-disabled-grep-db (apply oldfn args))))
(advice-add #'company-complete-common
            :around
            (lambda (oldfn &rest args)
              (custom/semantic/with-disabled-grep-db (apply oldfn args))))

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

;; Redefined because default implementation does not acquire databases
;; of project dependencies.
(defun semanticdb-find-translate-path-brutish-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((basedb
         (cond ((null path)
                semanticdb-current-database)
               ((semanticdb-table-p path)
                (oref path parent-db))
               (t (let ((tt (semantic-something-to-tag-table path)))
                    (if tt
                        ;; @todo - What does this DO ??!?!
                        ;; NOTE: Looks like some esoteric hack.
                        (with-current-buffer (semantic-tag-buffer (car tt))
                          semanticdb-current-database)
                      semanticdb-current-database))))))
    (apply
     #'nconc
     (mapcar
      (lambda (db)
        (let ((tabs (semanticdb-get-database-tables db))
              (ret nil))
          ;; Only return tables of the same language (major-mode)
          ;; as the current search environment.
          (while tabs
            (semantic-throw-on-input 'translate-path-brutish)
            (if (semanticdb-equivalent-mode-for-search (car tabs)
                                                       (current-buffer))
                (setq ret (cons (car tabs) ret)))
            (setq tabs (cdr tabs)))
          ret))
      (custom/delete-dups-eq
       (custom/append-new-backbone
        (semanticdb-current-database-list
         (if basedb
             (oref basedb reference-directory)
           default-directory))
        (apply #'custom/append-new-backbone
               (loop for root in (custom/ede/get-project-dependencies)
                     collect (semanticdb-current-database-list root)))))))))

;; Redefined because default implementation refused to list databases
;; not attached to registered EDE projects or explicitly stated in
;; semanticdb-project-roots.
(defun semanticdb-current-database-list (&optional dir)
  "Return a list of databases associated with the current buffer.
If optional argument DIR is non-nil, then use DIR as the starting directory.
If this buffer has a database, but doesn't have a project associated
with it, return nil.
First, it checks `semanticdb-project-root-functions', and if that
has no results, it checks `semanticdb-project-roots'.  If that fails,
it returns the results of function `semanticdb-current-database'.
Always append `semanticdb-project-system-databases' if
`semanticdb-search-system' is non-nil."
  (let ((root nil)			; found root directory
        (dbs nil)			; collected databases
        ;; All user roots + project dependencies.
        (roots (nconc semanticdb-project-roots
                      (custom/ede/get-project-dependencies)))
        (dir (file-truename (or dir default-directory))))
    ;; Find the root based on project functions.
    (setq root (run-hook-with-args-until-success
                'semanticdb-project-root-functions
                dir))
    (if root
        (setq root (file-truename root))
      ;; Else, Find roots based on strings
      (while roots
        (let ((r (file-truename (car roots))))
          (if (string-match (concat "^" (regexp-quote r)) dir)
              (setq root r)))
        (setq roots (cdr roots))))
    ;; If no roots are found, use this directory.
    (unless root (setq root dir))
    ;; Find databases based on the root directory.
    (when root
      ;; The rootlist allows the root functions to possibly
      ;; return several roots which are in different areas but
      ;; all apart of the same system.
      (let ((regexp (concat "^" (regexp-quote root)))
            (adb semanticdb-database-list)) ; all databases
        (while adb
          ;; I don't like this part, but close enough.
          (if (and (slot-boundp (car adb) 'reference-directory)
                   (string-match regexp (oref (car adb) reference-directory)))
              (setq dbs (cons (car adb) dbs)))
          (setq adb (cdr adb)))))
    ;; Add in system databases
    (when semanticdb-search-system-databases
      (setq dbs (nconc dbs semanticdb-project-system-databases)))
    ;; Return
    dbs))

;; Added more relaxed checking for types because old implementation
;; failed to infer type definition from forward type declaration.
(cl-defmethod semantic-analyze-refs-impl
  ((refs semantic-analyze-references) &optional in-buffer)
  "Return the implementations derived in the reference analyzer REFS.
Optional argument IN-BUFFER indicates that the returned tag should be in an active buffer."
  (let ((allhits (oref refs rawsearchdata))
        (tag (oref refs :tag))
        (impl nil))
    (semanticdb-find-result-mapc
     (lambda (T DB)
       "Examine T in the database DB, and sort it."
       (let* ((ans (semanticdb-normalize-one-tag DB T))
              (aT (cdr ans))
              (aDB (car ans)))
         (when (and (not (semantic-tag-prototype-p aT))
                    ;; When searching for type implementations,
                    ;; semantic-tag-similar-p is too strict, so we
                    ;; must use our simplified tag comparison.
                    (if (equal (semantic-tag-class tag) 'type)
                        (and (equal (semantic-tag-type tag)
                                    (semantic-tag-type aT))
                             (equal (semantic-tag-name tag)
                                    (semantic-tag-name aT)))
                      (semantic-tag-similar-p tag aT
                                              :prototype-flag
                                              :parent
                                              :typemodifiers
                                              :default-value)))
           (when in-buffer (save-excursion (semantic-go-to-tag aT aDB)))
           (push aT impl))))
     allhits)
    impl))

;; Fixed bad handling of macros. Bodies of macro definitions were not
;; detected by old implementation so ugly regex hack was used in order
;; to fix it.
(defun semantic-current-tag ()
  "Return the current tag in the current buffer.
If there are more than one in the same location, return the
smallest tag.  Return nil if there is no tag here."
  (if (or (equal major-mode 'c-mode)
          (equal major-mode 'c++-mode))
      (or (car (nreverse (semantic-find-tag-by-overlay)))
          (save-excursion
            (let* ((old-pos (point))
                   (tag (or (car (nreverse (semantic-find-tag-by-overlay)))
                            (semantic-find-tag-by-overlay-prev)))
                   (real-start nil))
              (when (semantic-tag-variable-constant-p tag)
                (goto-char (semantic-tag-start tag))
                (beginning-of-visual-line)
                (setq real-start (point))
                (save-match-data
                  (when (looking-at
                         (pcre-to-elisp
                          "\\s*#define\\s(?:[^\\\n]+\\\n)*[^\n]+\n"))
                    (goto-char (match-end 0))))
                (when (>= (1- (point)) old-pos)
                  ;; Readjust the tag overlay to reflect real size.
                  (semantic-tag-set-bounds tag
                                           real-start
                                           (1- (point)))
                  tag)))))
    (car (nreverse (semantic-find-tag-by-overlay)))))



(defun custom/semantic/enable-decorations-locally ()
  (when (and (featurep 'semantic)
             (semantic-active-p)
             ;; Ugly way how to check if mode is
             ;; open, unfortunately, variable
             ;; semantic-decoration-mode
             ;; cant be trusted. No idea why.
             (not (loop for o
                        in (with-current-buffer (current-buffer)
                             (semantic-overlays-in (point-min)
                                                   (point-max)))
                        if (semantic-decoration-p o)
                        return t)))
    (semantic-decoration-mode 1)))

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

(defun custom/semantic/show-parser-errors ()
  (interactive)
  (semantic-show-unmatched-syntax-mode 1)
  (message "Parser errors are shown as red underlines"))

(defun custom/semantic/hide-parser-errors ()
  (interactive)
  (semantic-show-unmatched-syntax-mode -1)
  (message "Parser errors are hidden"))

(defun custom/ede/load-config-file (proj-root &optional dont-ask)
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
                           (not dont-ask)
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
        (and (not dont-ask)
             (custom/ede/generate-config-file
              (file-name-as-directory proj-root)))))
    config))

(defun custom/ede/get-project-dependencies ()
  (let* ((root (semantic-symref-calculate-rootdir))
         (proj-config (if root
                          (custom/ede/load-config-file root t)))
         (source-roots (if proj-config
                           (custom/map/get "source-roots"
                                           proj-config))))
    source-roots))

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

;; Added no function args version of semanticdb-deep-find-tags-by-name.
;; We generally dont want to jump to some argument of some function somewhere.
(defun semanticdb-deep-no-args-find-tags-by-name
    (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-no-args-find-tags-by-name-method table name tags))
   path find-file-match))

;; Added no function args version of semanticdb-deep-find-tags-by-name-method.
;; Needed by semanticdb-deep-no-args-find-tags-by-name.
(cl-defmethod semanticdb-deep-no-args-find-tags-by-name-method
  ((table semanticdb-abstract-table) name &optional tags)
  "In TABLE, find all occurrences of tags with NAME.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-by-name name
                              (custom/semantic/flatten-tags-table
                               (or tags (semanticdb-get-tags table)))))

(defun semantic-ia-fast-jump (point &optional ext-ctx)
  "Jump to the tag referred to by the code at POINT.
Uses `semantic-analyze-current-context' output to identify an accurate
origin of the code at point."
  (interactive "d")
  ;; Reparse buffer when needed.
  (semantic-fetch-tags)
  (custom/semantic/load-all-project-dbs)
  (custom/semantic/with-disabled-grep-db
      (let* ((ctxt (or ext-ctx
                       (semantic-analyze-current-context point)))
             (pf (and ctxt (reverse (oref ctxt prefix))))
             ;; In the analyzer context, the PREFIX is the list of items
             ;; that makes up the code context at point.  Thus the c++ code
             ;; this.that().theothe
             ;; would make a list:
             ;; ( ("this" variable ..) ("that" function ...) "theothe")
             ;; Where the first two elements are the semantic tags of the prefix.
             ;;
             ;; PF is the reverse of this list.  If the first item is a string,
             ;; then it is an incomplete symbol, thus we pick the second.
             ;; The second cannot be a string, as that would have been an error.
             (pf-len (length pf))
             (first (car pf))
             (second (nth 1 pf)))
        (cond
         ((semantic-tag-p first)
          ;; We have a match.  Just go there.
          (semantic-ia--fast-jump-helper first))
         ((semantic-tag-p second)
          ;; Because FIRST failed, we should visit our second tag.
          ;; HOWEVER, the tag we actually want that was only an unfound
          ;; string may be related to some take in the datatype that belongs
          ;; to SECOND.  Thus, instead of visiting second directly, we
          ;; can offer to find the type of SECOND, and go there.
          (let ((secondclass (car (reverse (oref ctxt prefixtypes)))))
            (cond
             ((and (semantic-tag-with-position-p secondclass)
                   (y-or-n-p (format-message
                              "Could not find `%s'.  Jump to %s? "
                              first (semantic-tag-name secondclass))))
              (semantic-ia--fast-jump-helper secondclass))
             ;; If we missed out on the class of the second item, then
             ;; just visit SECOND.
             ((and (semantic-tag-p second)
                   (y-or-n-p (format-message
                              "Could not find `%s'.  Jump to %s? "
                              first (semantic-tag-name second))))
              (semantic-ia--fast-jump-helper second)))))
         ((semantic-tag-of-class-p (semantic-current-tag) 'include)
          ;; Just borrow this cool fcn.
          (require 'semantic/decorate/include)
          ;; Push the mark, so you can pop global mark back, or
          ;; use semantic-mru-bookmark mode to do so.
          (xref-push-marker-stack)
          (semantic-decoration-include-visit))
         ((and (equal pf-len 1)
               (stringp first))
          ;; Lets try to handle enums and macros. These things are ignored
          ;; by context analyzer so we must do it here.
          (custom/semantic/with-disabled-grep-db
              (let* ((tags (semanticdb-strip-find-results
                            (semanticdb-deep-no-args-find-tags-by-name
                             first
                             (current-buffer)
                             t)
                            t)))
                (if tags
                    (custom/semantic/goto-tag (custom/semantic/choose-tag tags))
                  ;; Last regard, use brute force.
                  (custom/semantic/complete-jump first)))))
         (t (error (concat "Could not find suitable jump point for '%s'"
                           " (try custom/semantic/complete-jump)!")
                   first))))))

(defun semantic-ia--fast-jump-helper (dest)
  "Jump to DEST, a Semantic tag.
This helper manages the mark, buffer switching, and pulsing."
  ;; We have a tag, but in C++, we usually get a prototype instead
  ;; because of header files.  Let's try to find the actual
  ;; implementation instead.
  (when (semantic-tag-prototype-p dest)
    (custom/semantic/with-enabled-grep-db
        (let* ((refs (semantic-analyze-tag-references dest))
               (impl (semantic-analyze-refs-impl refs t)))
          (when impl (setq dest (custom/semantic/choose-tag impl))))))
  ;; Make sure we have a place to go...
  (if (not (and (or (semantic-tag-with-position-p dest)
                    (semantic-tag-get-attribute dest :line))
                (semantic-tag-file-name dest)))
      (error "Tag %s has no buffer information"
             (semantic-format-tag-name dest)))
  (custom/semantic/goto-tag dest))

(defun semantic-analyze-proto-impl-toggle ()
  "Toggle between the implementation, and a prototype of tag under point."
  (interactive)
  (require 'semantic/decorate)
  (semantic-fetch-tags)
  (custom/semantic/load-all-project-dbs)
  (let* ((tag (semantic-current-tag))
         (sar (if tag
                  (semantic-analyze-tag-references tag)
                (error "Point must be in a declaration")))
         (target (if (semantic-tag-prototype-p tag)
                     (custom/semantic/choose-tag
                      (semantic-analyze-refs-impl sar t))
                   (custom/semantic/choose-tag
                    (semantic-analyze-refs-proto sar t)))))
    (when (not target)
      (error "Could not find suitable %s"
             (if (semantic-tag-prototype-p tag)
                 "implementation"
               "prototype")))
    (custom/semantic/goto-tag target)))

(defun custom/semantic/find-children-of-current-class ()
  (interactive)
  (semantic-fetch-tags)
  (custom/semantic/load-all-project-dbs)
  (let ((curr-tag (semantic-current-tag))
        (curr-parent-tag (semantic-current-tag-parent))
        (class-tag nil))
    (cond ((and curr-tag
                (semantic-tag-of-class-p curr-tag 'type)
                (member (semantic-tag-type curr-tag)
                        '("class" "struct")))
           (setq class-tag curr-tag))
          ((and curr-parent-tag
                (semantic-tag-of-class-p curr-parent-tag 'type)
                (member (semantic-tag-type curr-parent-tag)
                        '("class" "struct")))
           (setq class-tag curr-parent-tag))
          ((and curr-tag
                (equal (semantic-tag-class curr-tag) 'function))
           (setq class-tag (semantic-up-reference curr-tag)))
          ((and curr-parent-tag
                (semantic-tag-of-class-p curr-parent-tag 'function))
           (setq class-tag (semantic-up-reference curr-parent-tag)))
          (t
           (if curr-tag
               (error "Failed to find parent class of current tag!")
             (error "Failed to find parent class of current context!"))))
    (if (and (semantic-tag-of-class-p class-tag 'type)
             (member (semantic-tag-type class-tag) '("class" "struct")))
        (let ((tags (custom/semantic/get-subclasses class-tag)))
          (if tags
              (custom/semantic/goto-tag (custom/semantic/choose-tag tags))
            (error "Failed to find children of '%s'!"
                   (semantic-tag-name class-tag))))
      (error "Failed to find parent class of current tag!"))))

(defun semantic-symref-symbol (sym)
  "Find references to the symbol SYM.
This command uses the currently configured references tool within the
current project to find references to the input SYM.  The
references are organized by file and the name of the function
they are used in.
Display the references in `semantic-symref-results-mode'."
  (interactive (list (custom/semantic/with-disabled-grep-db
                         (custom/semantic-complete-read-tag-project
                          "Find references for: "
                          nil
                          (thing-at-point 'symbol)))))
  (semantic-fetch-tags)
  ;; Gather results and tags
  (message "Gathering References...")
  (let ((res (semantic-symref-find-references-by-name sym)))
    (semantic-symref-produce-list-on-results res sym)))

;; In compared to semantic-symref-result-get-tags, it does not strip
;; out duplicates and populate hit property of tags.
(cl-defmethod semantic-symref-result-get-tags-as-is ((result semantic-symref-result)
                                                     &optional open-buffers)
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
        (setq semantic-symref-recently-opened-buffers nil))
      (loop for T in ans if T collect T))))

(defun custom/semantic/ctxt-current-symbol-and-bounds-detached (expr)
  (let ((parent-s-table semantic-lex-syntax-table))
    (with-temp-buffer
      (setq semantic-lex-syntax-table parent-s-table)
      (insert expr)
      (semantic-ctxt-current-symbol-and-bounds))))

(defun custom/semantic/analyze-current-context-detached (expr
                                                         &optional position)
  (when (null position)
    (setq position (point)))
  (let* ((semantic-analyze-error-stack nil)
         (prefixandbounds
          (custom/semantic/ctxt-current-symbol-and-bounds-detached expr))
         (prefix (car prefixandbounds))
         (bounds (nth 2 prefixandbounds))
         ;; @todo - vv too early to really know this answer! vv
         (prefixclass '(function variable type))
         (prefixtypes nil)
         (scope (semantic-calculate-scope position))
         newseq)
    ;; Only do work if we have bounds (meaning a prefix to complete)
    (when bounds
      (if debug-on-error
          (catch 'unfindable
            (setq prefix (semantic-analyze-find-tag-sequence
                          prefix scope 'prefixtypes 'unfindable))
            ;; If there's an alias, dereference it and analyze
            ;; sequence again.
            (when (setq newseq
                        (semantic-analyze-dereference-alias prefix))
              (setq prefix (semantic-analyze-find-tag-sequence
                            newseq scope 'prefixtypes 'unfindable))))
        ;; Debug on error is off.  Capture errors and move on
        (condition-case err
            ;; NOTE: This line is duplicated in
            ;;       semantic-analyzer-debug-global-symbol
            ;;       You will need to update both places.
            (progn
              (setq prefix (semantic-analyze-find-tag-sequence
                            prefix scope 'prefixtypes))
              (when (setq newseq
                          (semantic-analyze-dereference-alias prefix))
                (setq prefix
                      (semantic-analyze-find-tag-sequence newseq
                                                          scope
                                                          'prefixtypes))))
          (error (semantic-analyze-push-error err))))
      (semantic-analyze-context "context"
                                :buffer (current-buffer)
                                :scope scope
                                :bounds bounds
                                :prefix prefix
                                :prefixclass prefixclass
                                :prefixtypes prefixtypes
                                :errors semantic-analyze-error-stack))))

(defun custom/semantic/complete-jump (sym)
  (interactive (list (custom/semantic/with-disabled-grep-db
                         (custom/semantic-complete-read-tag-project
                          "Look for symbol: "
                          nil
                          (thing-at-point 'symbol)))))
  (if (> (length
          (car
           (custom/semantic/ctxt-current-symbol-and-bounds-detached sym)))
         1)
      (let ((ctx (custom/semantic/best-tag-user-assist-enabled type+user
                     (custom/semantic/analyze-current-context-detached
                      sym))))
        (if ctx
            (semantic-ia-fast-jump (point) ctx)
          (error "Failed to construct detached context")))
    ;; Use modified symref module for getting all tags with target name in
    ;; current project and all its dependencies.
    ;; Bypasses bug when brute deep searching all tables in project
    ;; using standard semantic find routines.
    ;;
    ;; NOTE: That bug was probably caused by GNU Global which was fired
    ;; because it SUCKED.
    (custom/semantic/with-enabled-grep-db
        (let ((tags (or (let ((res (semantic-symref-find-tags-by-name sym)))
                          (if res
                              (semantic-symref-result-get-tags-as-is res)))
                        ;; When symrefing fails, it means it might be in system
                        ;; dependencies, use full scope search then.
                        (custom/semantic/best-tag-user-assist-enabled user
                            (semantic-analyze-find-tag sym))
                        ;; Still nothing? Maybe it is in local variables.
                        (semantic-find-tags-by-name
                         sym
                         (custom/semantic/get-local-variables)))))
          (when (semantic-tag-p tags)
            (setq tags (list tags)))
          (if tags
              (let* ((chosen-tag (custom/semantic/choose-tag tags)))
                (custom/semantic/goto-tag chosen-tag))
            (message "No tags found for %s" sym))))))

(defvar-local custom/semantic/is-prefix-pointer-state (cons nil 'allow))
(defun custom/semantic/is-prefix-pointer-p (&optional point)
  (interactive "d")
  (if (not point)
      (setq point (point)))
  (when (and (not (custom/pos-is-in-comment point))
             (not (custom/pos-is-in-string point))
             (semantic-current-tag)
             (equal (semantic-tag-class (semantic-current-tag))
                    'function)
             (not (semantic-tag-prototype-p (semantic-current-tag))))
    (let* ((guess-if-pointer
            (lambda (sym)
              (if sym
                  (let ((case-fold-search nil))
                    (s-match (pcre-to-elisp/cached "^p[A-Z].*") sym)))))
           (ctx-sym (cadr (semantic-ctxt-current-symbol-and-bounds point)))
           (curr-fun-name (custom/semantic/get-current-function-name))
           (decision (not
                      (and
                       (equal curr-fun-name
                              (car custom/semantic/is-prefix-pointer-state))
                       (not (equal (cdr custom/semantic/is-prefix-pointer-state)
                                   'allow)))))
           (ctx-with-time
            (custom/with-measure-time
                (if decision
                    (ignore-errors
                      (custom/semantic/with-disabled-grep-db
                          (semantic-analyze-current-context point))))))
           (ctx (car ctx-with-time))
           (run-time (cdr ctx-with-time))
           (prefix (if ctx (oref ctx prefix) nil))
           (last-pref (car (last prefix))))
      (when (> run-time 20)
        (setq custom/semantic/is-prefix-pointer-state
              (cons curr-fun-name 'no-ctx-fetch))
        (when (yes-or-no-p
               (format
                (concat "Context fetch was too slow (%s) so it was "
                        "disabled temporary for this function (%s). Do "
                        "you want to enable type guessing (pSomething "
                        "is considered as a pointer)?")
                run-time
                curr-fun-name))
          (setq custom/semantic/is-prefix-pointer-state
                (cons curr-fun-name 'guess))))
      (if last-pref
          (if (semantic-tag-p last-pref)
              (semantic-tag-get-attribute last-pref
                                          :pointer)
            (if (equal (cdr custom/semantic/is-prefix-pointer-state)
                       'guess)
                (funcall guess-if-pointer ctx-sym)))
        (if (equal (cdr custom/semantic/is-prefix-pointer-state)
                   'guess)
            (funcall guess-if-pointer ctx-sym))))))

;;;; TAG ACCESS AND VARIOUS INDEX QUERIES
;; CODE:

;; Return eieio object with analyzed current context.
;; For inspection, use custom/inspect-eieio.
;;     (semantic-analyze-current-context)
;;
;; Return eieio object with analyzed current scope.
;; It is functional subset of semantic-analyze-current-context.
;;     (semantic-calculate-scope)
;;
;; Control tag sources when quering DB
;;     (semanticdb-find-default-throttle)
;;
;; Add preprocesor value for all projects
;;     (semantic-c-add-preprocessor-symbol "__SYM__" "VAL")
;;
;; System includes are not project includes, these apply for
;; every project (and are not saved)
;;     (semantic-add-system-include include-root-dir symbol-for-mode)
;;     (semantic-remove-system-include include-root-dir symbol-for-mode)

;; Overrided because default implementation does not flatten tag
;; hierarchy (ie. deep search). As an result, all classes and struct
;; inside namespaces were skipped from search. That sucked.
(cl-defmethod semanticdb-find-tags-subclasses-of-type-method
  ((table semanticdb-abstract-table) parent &optional tags)
  "In TABLE, find all occurrences of tags whose parent is the PARENT type.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (require 'semantic/find)
  (semantic-find-tags-subclasses-of-type parent
                                         (or tags
                                             (custom/semantic/flatten-tags-table
                                              (semanticdb-get-tags table)))))

(defun custom/semantic/get-local-variables (&optional point)
  (interactive "d")
  (let ((tags (semantic-get-all-local-variables)))
    (if (called-interactively-p 'any)
        (if tags
            (custom/with-simple-pop-up "*FUNCTION VARS*"
              (setq kill-on-quit t)
              (dolist (tag tags)
                (insert (semantic-format-tag-prototype tag nil t)
                        "\n")))
          (error "Failed to find local variables!"))
      tags)))

(defun custom/semantic/get-superclasses (tag)
  (when (and (semantic-tag-p tag)
             (semantic-tag-of-class-p tag 'type)
             (or (semantic-tag-type-superclasses tag)
                 (semantic-tag-type-interfaces tag)))
    (let ((tag-buffer (semantic-tag-buffer tag))
          (tag-start (if (semantic-tag-with-position-p tag)
                         (semantic-tag-start tag))))
      (when (and tag-buffer tag-start)
        (with-current-buffer tag-buffer
          (save-excursion
            (goto-char tag-start)
            (let ((scope (semantic-calculate-scope tag-start))
                  (parents (or (semantic-tag-type-superclasses tag)
                               (semantic-tag-type-interfaces tag))))
              (loop for parent in parents
                    collect (custom/semantic/best-tag-user-assist-enabled type+user
                                (semantic-analyze-find-tag parent
                                                           'type
                                                           scope))))))))))

(defun custom/semantic/get-subclasses (tag)
  (when (and (semantic-tag-p tag)
             (semantic-tag-of-class-p tag 'type)
             (member (semantic-tag-type tag) '("class" "struct")))
    (let* ((tagname (semantic-tag-name tag))
           ;; Make sure grep backend is enabled.
           (semantic-symref-tool 'grep)
           ;; Load all files with mentioned class name into memory
           ;; and their DBs from persistence.
           ;;
           ;; Extremely important step, otherwise only currently
           ;; loaded databases will be searched.
           (_ (semantic-symref-find-tags-by-name tagname))
           (result (semanticdb-find-tags-subclasses-of-type tagname
                                                            nil
                                                            t)))
      (semanticdb-strip-find-results result t))))

(defun custom/semantic/search-db (sym &optional
                                      buffer
                                      find-file-match)
  (let ((tags (semanticdb-find-tags-by-name sym
                                            buffer
                                            find-file-match)))
    (if find-file-match
        (semanticdb-strip-find-results tags find-file-match)
      (semanticdb-fast-strip-find-results tags))))

(defun custom/semantic/search-db-deep (sym &optional
                                           buffer
                                           find-file-match)
  (let ((tags (semanticdb-deep-find-tags-by-name sym
                                                 buffer
                                                 find-file-match)))
    (if find-file-match
        (semanticdb-strip-find-results tags find-file-match)
      (semanticdb-fast-strip-find-results tags))))

(defun custom/semantic/search-db-brute-deep (sym &optional find-file-match)
  (let ((tags (semanticdb-brute-deep-find-tags-by-name sym
                                                       nil
                                                       find-file-match)))
    (if find-file-match
        (semanticdb-strip-find-results tags find-file-match)
      (semanticdb-fast-strip-find-results tags))))


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
  (let* ((table (semantic-something-to-tag-table table))
         (lists (list table)))
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

(defun custom/semantic/search-file-no-includes (symbol &optional file)
  (when (not file)
    (setq file (current-buffer)))
  (with-current-buffer (if (bufferp file)
                           file
                         (find-file-noselect file))
    (save-excursion
      (let ((tags (custom/semantic/flatten-tags-table
                   (semantic-fetch-tags))))
        (semantic-find-tags-by-name symbol tags)))))

(defun custom/semantic/get-tag-code-symbols (tag &optional collector)
  (interactive (list (semantic-current-tag)))
  (with-current-buffer (semantic-tag-buffer tag)
    (save-excursion
      (let* ((tokens (if (equal (semantic-tag-class tag)
                                'function)
                         (semantic-lex (save-excursion
                                         (goto-char (semantic-tag-start tag))
                                         (search-forward "{"))
                                       (semantic-tag-end tag)
                                       99)))
             (symbols (if collector
                          (loop for token in tokens
                                for type = (car token)
                                for range = (cdr token)
                                if (equal type 'symbol)
                                do (funcall collector
                                            (buffer-substring-no-properties
                                             (car range)
                                             (cdr range))))
                        (loop for token in tokens
                              for type = (car token)
                              for range = (cdr token)
                              if (equal type 'symbol)
                              collect (buffer-substring-no-properties
                                       (car range)
                                       (cdr range))))))
        (if (called-interactively-p)
            (custom/with-simple-pop-up "*Semantic Tag Code Symbols*"
              (setq kill-on-quit t)
              (loop for sym in symbols
                    do (insert sym "\n")))
          symbols)))))

(defun custom/semantic/get-tag-code-symbols-as-map (tag)
  (let ((map (@dict-new))
        (collector (lambda (sym) (@dict-set sym t map))))
    (custom/semantic/get-tag-code-symbols tag collector)
    map))

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
  (if (equal (length tags) 1)
      (car tags)
    (when (not (equal (length tags) 0))
      (let* ((tag-summary-f
              (lambda (tag)
                (format "%s = %s"
                        (let* ((str
                                (concat
                                 (if (semantic-tag-prototype-p tag)
                                     "[prototype] "
                                   "")
                                 (semantic-format-tag-prototype tag nil t)))
                               (real-len (length str))
                               (limit-len 97))
                          (if (> real-len limit-len)
                              (concat (substring str 0 limit-len)
                                      "...")
                            str))
                        (let* ((str (buffer-file-name
                                     (semantic-tag-buffer tag)))
                               (real-len (length str))
                               (limit-len 77))
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
             (summaries
              (loop for tag in tags
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
        chosen-tag))))

(defun custom/semantic/goto-tag (tag)
  (custom/universal-push-mark)
  (find-file (buffer-file-name (semantic-tag-buffer tag)))
  (semantic-go-to-tag tag)
  (if (not semantic-idle-scheduler-mode)
      (semantic-idle-scheduler-mode))
  (semantic-refresh-tags-safe)
  ;; Maybe database was from some reason outdated, we need to somehow
  ;; hide that from user and land exactly where he wants.
  (let* ((curr-pos (point))
         (curr-name (semantic-tag-name tag))
         (enclosing-tag (semantic-current-tag))
         (enclosing-tag-name (if enclosing-tag
                                 (semantic-tag-name enclosing-tag)))
         (expected-pos (if enclosing-tag
                           (semantic-tag-start enclosing-tag)))
         (candidate-tags nil)
         (nearest-tag nil)
         (nearest-tag-distance 0)
         (temp-distance 0)
         (temp-distance2 0))
    (cond ((and (equal curr-pos expected-pos)
                (equal curr-name enclosing-tag-name))
           ;; All is ok.
           )
          ;; Local variables have no overlays thus no enclosing tag can
          ;; be found for them. In order to correctly detect valid
          ;; landing for local variables, we must iterate through all local
          ;; variables in current scope and check if one of them match
          ;; to visited tag.
          ((loop for var-tag in (semantic-get-local-variables)
                 if (and (equal curr-pos (semantic-tag-start var-tag))
                         (equal curr-name (semantic-tag-name var-tag)))
                 return t)
           ;; All is ok.
           )
          ((equal curr-name enclosing-tag-name)
           ;; We are not exactly ok, but we landed somehow somewhere
           ;; in correct tag, just readjust.
           (goto-char expected-pos)
           (setq tag enclosing-tag))
          (t
           ;; All is wrong, we landed completely somewhere else.
           ;; As an fix, list all tags with same name and jump to
           ;; nearest one.
           (setq candidate-tags
                 (custom/semantic/search-file-no-includes curr-name))
           (loop for candidate-tag in candidate-tags
                 do (progn
                      (setq temp-distance
                            (abs (- curr-pos
                                    (semantic-tag-start candidate-tag)))
                            temp-distance2
                            (abs (- curr-pos
                                    (semantic-tag-end candidate-tag))))
                      (when (< temp-distance2 temp-distance)
                        (setq temp-distance temp-distance2))
                      (if nearest-tag
                          (when (<= temp-distance nearest-tag-distance)
                            (setq nearest-tag-distance temp-distance
                                  nearest-tag candidate-tag))
                        (setq nearest-tag-distance temp-distance
                              nearest-tag candidate-tag))))
           (if nearest-tag
               (setq tag nearest-tag)
             (setq tag nil))
           (if nearest-tag
               ;; We found tag with same name which is nearest from
               ;; our position of arrival.
               (goto-char (semantic-tag-start nearest-tag))
             ;; We failed to find nearest tag. Not good. We must
             ;; resort to solution based on search-forward and search-backward.
             ;; Choose the nearest.
             (let ((f-pos (save-excursion (search-forward curr-name nil t)))
                   (b-pos (save-excursion (search-backward curr-name nil t))))
               (cond ((and f-pos b-pos)
                      (if (< (abs (- curr-pos f-pos))
                             (abs (- curr-pos b-pos)))
                          (goto-char f-pos)
                        (goto-char b-pos)))
                     (f-pos
                      (goto-char f-pos))
                     (b-pos
                      (goto-char b-pos))
                     (t
                      ;; Sorry, DB had really shitty data.
                      (error (concat "Invalid DB record was "
                                     "found and recovery failed!")))))))))
  (recenter)
  (when tag
    (pulse-momentary-highlight-region (semantic-tag-start tag)
                                      (semantic-tag-end tag))))

;;;; INDEX MANAGEMENT CODE
;; CODE:

(defun custom/semantic/index-directory (root &optional logger selection-regex)
  (interactive (list (ido-read-directory-name "Select directory: ")))
  (when (not selection-regex)
    (setq selection-regex
          (pcre-to-elisp/cached ".*\\.(?:c|cpp|h|hpp|cxx|hxx)$")))
  (when (not logger)
    (setq logger (lambda (_))))
  (let ((root (file-name-as-directory (file-truename root)))
        (files (directory-files root t)))
    (setq files (delete (format "%s." root) files))
    (setq files (delete (format "%s.." root) files))
    (setq files (delete (format "%s.git" root) files))
    (setq files (delete (format "%s.hg" root) files))
    (semanticdb-save-all-db)
    (loop for file in files
          do (if (not (file-accessible-directory-p file))
                 (when (string-match-p selection-regex file)
                   (if (ignore-errors
                         (semanticdb-file-table-object file))
                       (funcall logger (format "Parsing %s [OK]" file))
                     (funcall logger (format "Parsing %s [ERROR]" file))))
               (progn (semanticdb-save-all-db)
                      (custom/semantic/index-directory file
                                                       logger
                                                       selection-regex))))))

(defun custom/semantic/index-current-project ()
  (interactive)
  (let ((roots (cons (semantic-symref-calculate-rootdir)
                     (append (if (yes-or-no-p (concat "Do you want to index "
                                                      "global system include "
                                                      "paths (on linux it would"
                                                      " be a loooot paths...)?"))
                                 semantic-dependency-system-include-path)
                             (custom/ede/get-project-dependencies)))))
    (if roots
        (progn
          (message "Indexing: %s" roots)
          (let* ((buffname "*Semantic Indexer Log*")
                 (buff (or (custom/get-buffer buffname)
                           (generate-new-buffer buffname)))
                 (logger (lambda (msg)
                           (insert msg "\n")
                           (goto-char (point-max))
                           (redisplay t))))
            (switch-to-buffer buff)
            (erase-buffer)
            (redisplay t)
            (loop for root in roots
                  do (custom/semantic/index-directory root logger))
            (insert "\n" "DONE")
            (read-only-mode t)))
      (error "Failed to find roots, are you in project?"))))

(setq custom/semantic/parse-table-queue nil)
(defun custom/semantic/parser-executor ()
  (when custom/semantic/parse-table-queue
    (semanticdb-refresh-table
     (pop custom/semantic/parse-table-queue) t)))

(defun custom/semantic/refresh-table-in-future (table)
  (push table custom/semantic/parse-table-queue)
  (semantic-idle-scheduler-add #'custom/semantic/parser-executor))

(setq custom/semantic/loaded-projects (@dict-new))
(defun custom/semantic/load-all-project-dbs ()
  (interactive)
  (when (not (@at custom/semantic/loaded-projects
                  (semantic-symref-calculate-rootdir)))
    (let* ((proj-roots (cons (semantic-symref-calculate-rootdir)
                             (custom/ede/get-project-dependencies)))
           (store-path (file-name-as-directory
                        semanticdb-default-save-directory))
           (check-regexp (concat "^\\(?:"
                                 (s-join "\\|"
                                         (loop for proj-root in proj-roots
                                               collect
                                               (regexp-quote proj-root)))
                                 "\\)"))

           (raw-cache-paths (directory-files store-path))
           (cache-paths (loop for raw-cache in raw-cache-paths
                              for cache = (cedet-file-name-to-directory-name
                                           raw-cache)
                              if (s-match check-regexp cache)
                              collect raw-cache)))
      (loop for cache in cache-paths
            for full-path = (expand-file-name (concat store-path cache))
            for decoded-cache = (cedet-file-name-to-directory-name cache)
            if (not (semanticdb-file-loaded-p full-path))
            do (progn
                 (message "Loading: %s" full-path)
                 (let* ((db (semanticdb-load-database full-path))
                        (tables (if db (semanticdb-get-database-tables db))))
                   (when db
                     (oset db
                           reference-directory
                           (s-replace-regexp "semantic.cache"
                                             ""
                                             decoded-cache)))
                   (loop for table in tables
                         if (and (file-exists-p
                                  (concat (oref db reference-directory)
                                          (oref table file)))
                                 (semanticdb-needs-refresh-p table))
                         do (custom/semantic/refresh-table-in-future table))))))
    (@dict-set (semantic-symref-calculate-rootdir)
               t
               custom/semantic/loaded-projects)))

(defun custom/semantic/get-current-db ()
  (semanticdb-directory-loaded-p
   (file-name-directory buffer-file-name)))

(defun custom/semantic/get-current-table ()
  (let ((db (custom/semantic/get-current-db)))
    (when db
      (semanticdb-file-table db buffer-file-name))))

(defun custom/semantic/dump-db-refpaths ()
  (interactive)
  (custom/with-simple-pop-up "*Semantic Db Ref Paths*"
    (setq kill-on-quit t)
    (loop for db in semanticdb-database-list
          collect (insert (or (ignore-errors
                                (format " PATH: %s"
                                        (oref db
                                              reference-directory)))
                              (format "CLASS: %s"
                                      (eieio-object-class-name db)))
                          "\n"))))

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
    ;; TODO: Find match string on current line.
    ;;
    ;; semantic-displayor-traditional vanished so it does not
    ;; work anyway...
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
;; search backend too.
(defun custom/semantic-symref-grep-use-template-around (oldfn
                                                        rootdir
                                                        filepattern
                                                        flags
                                                        pattern)
  (setq rootdir (s-join " "
                        (cons rootdir
                              (custom/ede/get-project-dependencies))))
  (let ((res (funcall oldfn rootdir filepattern flags pattern)))
    ;; (message "Generated grep command: %s" res)
    res))

(advice-add #'semantic-symref-grep-use-template
            :around #'custom/semantic-symref-grep-use-template-around)

;;;; GREP VIRTUAL DATABASE HACKS
;; CODE:

;; Fix for too strict symref excludes for C and C++.
(setq semantic-symref-filepattern-alist
      (loop for entry in semantic-symref-filepattern-alist
            for label = (car entry)
            if (or (equal label 'c-mode)
                   (equal label 'c++-mode))
            collect `(,label "*.[chCH]" "*.[ch]pp" "*.cc" "*.hh")
            else
            collect entry))

(defun custom/gen-win-grep-emulation-cmd (searchfor)
  (when (eq system-type 'windows-nt)
    (custom/replace-all
     `(,(cons "$dirlist"
              (s-join ";"
                      (cons (semantic-symref-calculate-rootdir)
                            (custom/ede/get-project-dependencies))))
       ,(cons "$searchfor" searchfor)
       ,(cons "$filters" "*.c *.cpp *.cxx *.h *.hpp *.hxx"))
     "findstr /s /n /d:$dirlist \"$searchfor\" $filters")))

(defun custom/cleanse-findstr-output (buff)
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (pcre-to-elisp/cached
                   (concat "^(?:"
                           "\\s+([\\w\\\\/\\._\\-:]+):"
                           "|"
                           "([^\\s][\\w\\\\/\\._\\-:]+)(:\\d+:)([^\n]*)"
                           ")\n")))
          (prefix nil))
      (while (search-forward-regexp regexp nil t)
        (let ((root-path (match-string 1))
              (hit-path (match-string 2))
              (hit-line (match-string 3))
              (hit-payload (match-string 4))
              )
          (if root-path
              (progn
                (setq prefix (s-replace-regexp "\\\\" "/" root-path))
                (replace-match ""))
            (progn
              (if (not prefix)
                  (error (concat "Error during normalization of "
                                 "findstr result at line %s "
                                 "(missing prefix)!")
                         (line-number-at-pos (match-beginning 0))))
              (replace-match (concat prefix
                                     (s-replace-regexp (regexp-quote "\\")
                                                       "/"
                                                       hit-path)
                                     hit-line
                                     (s-replace-regexp (regexp-quote "\\")
                                                       "/"
                                                       hit-payload)
                                     "\n")))))))))

(defun custom/filter-raw-grep-output (hits searchfor)
  (setq custom/TAGNAME-HITS hits)
  (let ((deep-search #'custom/semantic/search-file-no-includes)
        (files (@dict-new))
        (result nil))
    (loop for (line . path) in hits
          do (@dict-set path t files))
    (@map (lambda (filename dummy)
            (setq result
                  (append result
                          (loop for tag
                                in (funcall deep-search
                                            searchfor
                                            filename)
                                collect (cons tag filename)))))
          files)
    (setq result
          (loop for (tag . file) in result
                collect (cons (with-current-buffer (find-file-noselect file)
                                (line-number-at-pos
                                 (semantic-tag-start tag)))
                              file)))
    (setq custom/TAGNAME-RESULT result)
    result))

(setq custom/semantic/grep-db-enabled t)

(defmacro custom/semantic/with-disabled-grep-db (&rest forms)
  (declare (indent 99) (debug t))
  `(let ((custom/semantic/grep-db-enabled nil))
     ,@forms))

(defmacro custom/semantic/with-enabled-grep-db (&rest forms)
  (declare (indent 99) (debug t))
  `(let ((custom/semantic/grep-db-enabled t))
     ,@forms))

(cl-defmethod semantic-symref-perform-search ((tool semantic-symref-tool-grep))
  "Perform a search with Grep."
  (when custom/semantic/grep-db-enabled
    ;; Grep doesn't support some types of searches.
    (when (not (memq (oref tool searchtype)
                     '(symbol regexp tagname)))
      (error "Symref impl GREP does not support searchtype of '%s' for '%s'!"
             (oref tool searchtype)
             (oref tool searchfor)))
    (message "Grepping (%s) for '%s'."
             (oref tool searchtype)
             (oref tool searchfor))
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
                                                      greppat))
              ;; Ugly hack for windows. Grep combined with find is
              ;; awfully slow on windows so we must resort to native
              ;; alternative which is fast.
              (wincmd (custom/gen-win-grep-emulation-cmd (oref tool
                                                               searchfor))))
          (if wincmd
              (progn (setq custom/GREP-CMD wincmd)
                     (call-process semantic-symref-grep-shell nil b nil
                                   shell-command-switch wincmd)
                     (custom/cleanse-findstr-output b))
            (progn (setq custom/GREP-CMD cmd)
                   (call-process semantic-symref-grep-shell nil b nil
                                 shell-command-switch cmd)))))
      (setq ans (semantic-symref-parse-tool-output tool b))
      ;; Special handling for search type tagname.
      ;; Semantic expect only positions of tags will be provided so we
      ;; must filter raw output from grep using semantic parsing
      ;; infrastructure.
      (message "Grep found %s hits."
               (length ans))
      (if (equal (oref tool searchtype) 'tagname)
          (custom/filter-raw-grep-output
           ans
           (oref tool searchfor))
        ans))))

;; Disable grepping in idle summary mode. We need just prototype here,
;; no need to scan whole project by grep.
(advice-add 'semantic-idle-summary-idle-function :around
            (lambda (oldfn)
              (custom/semantic/with-disabled-grep-db (funcall oldfn))))

;;;; CODE DOC ACQUISITION
;; CODE:

(defun semantic-ia-show-doc (point)
  "Display the code-level documentation for the symbol at POINT."
  (interactive "d")
  (let* ((actual-buffer (current-buffer))
         (ctxt (semantic-analyze-current-context point))
         (pf (and ctxt (reverse (oref ctxt prefix))))
         (tag (car pf)))
    (when (and (equal (length pf) 1)
               (stringp tag))
      (custom/semantic/with-disabled-grep-db
          (let ((tags (semanticdb-strip-find-results
                       (semanticdb-deep-find-tags-by-name tag
                                                          (current-buffer)
                                                          t)
                       t)))
            (if tags
                (setq tag (custom/semantic/choose-tag tags))))))
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
             (other-tags (if (semantic-tag-prototype-p tag)
                             (semantic-analyze-refs-impl sar t)
                           (semantic-analyze-refs-proto sar t)))
             (doc (or (semantic-documentation-for-tag tag) ""))
             (other-doc (or (if other-tags
                                (custom/longest-string
                                 (loop for other-tag in other-tags
                                       collect (semantic-documentation-for-tag
                                                other-tag))))
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
        (if (and (or (null doc)
                     (string= doc ""))
                 (or (null other-doc)
                     (string= other-doc "")))
            (message "Doc unavailable!")
          (custom/with-simple-pop-up (format "*Semantic DocView (%s)*"
                                             (semantic-tag-name tag))
            (setq kill-on-quit nil)
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
     (t (message "No tag found.")))))

(defun custom/semantic/find-type-tags-of-var (tag &optional buffer)
  (if (not buffer)
      (setq buffer (current-buffer)))
  (custom/semantic/with-disabled-grep-db
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
         (selected-tag (custom/semantic/choose-tag tags)))
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
           ;; semantic-find-tag-by-overlay-prev
           (starttag (senator-previous-tag-or-parent tag-start))
           (start (if starttag
                      (if (custom/val-in-range tag-start starttag)
                          (semantic-tag-start starttag)
                        (semantic-tag-end starttag))
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
                          "@param(?:\\[([^\\]]*)\\])?[ \t]*(\\w+)[ \t]*")
                         "Parameter[\\1] \\2 = ")
       (s-replace-regexp (pcre-to-elisp/cached
                          "^//\\s*")
                         "")
       (s-replace-regexp (pcre-to-elisp/cached
                          "^\\*+$")
                         "")
       (s-replace-regexp (pcre-to-elisp/cached
                          "^\\*\\s*(.+)\\s*\\*$")
                         "\\1")
       (s-replace-regexp (pcre-to-elisp/cached
                          "^\\*\\s*(.+)\\s*$")
                         "\\1")
       (s-replace-regexp (pcre-to-elisp/cached
                          "(\\s)@p\\s+([^\\s\\.]+)([\\s\\.])")
                         "\\1[param: \\2]\\3")))))

;;;; MINIBUFFER TAG AUTOCOMPLETION
;; CODE:

(defclass custom/semantic/collector-project-chained
  (semantic-collector-project-abstract)
  ()
  "Context aware completion engine for tags in a project.")

(cl-defmethod semantic-collector-calculate-completions-raw
  ((obj custom/semantic/collector-project-chained) prefix completionlist)
  "Calculate the completions for prefix from completionlist."
  (with-current-buffer (oref obj buffer)
    (let* ((table semanticdb-current-table)
           (ctx (custom/semantic/with-disabled-grep-db
                    (custom/semantic/best-tag-user-assist-enabled type+user
                        (custom/semantic/analyze-current-context-detached
                         prefix))))
           (result (semantic-analyze-possible-completions ctx 'no-unique)))
      (if result
          (list (cons table result))))))

;; Fixed invalid usage of semantic-analyze-current-context which
;; happened to be outside of semantic buffer.
;; Also it was wrapped in error catcher because abstract class has no
;; context slot.
;; Added check if slot 'first-pass-completions' exists before usage.
;; Reworked usage of 'try-completion' in order to support dot chains.
(cl-defmethod semantic-collector-calculate-completions
  ((obj semantic-collector-abstract) prefix partial)
  "Calculate completions for prefix as setup for other queries."
  (let* ((case-fold-search semantic-case-fold)
         (same-prefix-p (semantic-collector-last-prefix= obj prefix))
         (last-prefix (and (slot-boundp obj 'last-prefix)
                           (oref obj last-prefix)))
         (completionlist
          (cond ((or same-prefix-p
                     (and last-prefix (eq (compare-strings
                                           last-prefix 0 nil
                                           prefix 0 (length last-prefix)) t)))
                 ;; We have the same prefix, or last-prefix is a
                 ;; substring of the of new prefix, in which case we are
                 ;; refining our symbol so just re-use cache.
                 (oref obj last-all-completions))
                ((and last-prefix
                      (> (length prefix) 1)
                      (eq (compare-strings
                           prefix 0 nil
                           last-prefix 0 (length prefix)) t))
                 ;; The new prefix is a substring of the old
                 ;; prefix, and it's longer than one character.
                 ;; Perform a full search to pull in additional
                 ;; matches.
                 (with-current-buffer (oref obj buffer)
                   (custom/semantic/with-disabled-grep-db
                       (let ((context (semantic-analyze-current-context (point))))
                         ;; Set new context and make first-pass-completions
                         ;; unbound so that they are newly calculated.
                         (when (slot-exists-p obj context)
                           (oset obj context context))
                         (when (and (slot-exists-p obj 'first-pass-completions)
                                    (slot-boundp obj 'first-pass-completions))
                           (slot-makeunbound obj 'first-pass-completions)))))
                 nil)))
         ;; Get the result
         (answer (if same-prefix-p
                     completionlist
                   (semantic-collector-calculate-completions-raw
                    obj prefix completionlist)))
         (completion nil)
         (complete-not-uniq nil))
    ;;(semanticdb-find-result-test answer)
    (when (not same-prefix-p)
      ;; Save results if it is interesting and beneficial
      (oset obj last-prefix prefix)
      (oset obj last-all-completions answer))
    ;; Now calculate the completion.
    (let ((prefix-segments
           (car
            (with-current-buffer (oref obj buffer)
              (custom/semantic/ctxt-current-symbol-and-bounds-detached
               prefix))))
          (candidates (loop for tag in (semanticdb-strip-find-results answer)
                            collect (semantic-tag-name tag)))
          short-prefix)
      (when (> (length prefix-segments) 1)
        (setq short-prefix
              (s-join "." (@init prefix-segments)))
        (setq candidates
              (loop for c in candidates
                    collect (concat short-prefix "." c))))
      (setq completion (try-completion prefix candidates)))
    (oset obj last-whitespace-completion nil)
    (oset obj current-exact-match nil)
    ;; Only do this if a completion was found.  Letting a nil in
    ;; could cause a full semanticdb search by accident.
    (when completion
      (oset obj last-completion
            (cond
             ;; Unique match in AC.  Last completion is a match.
             ;; Also set the current-exact-match.
             ((eq completion t)
              (oset obj current-exact-match answer)
              prefix)
             ;; It may be complete (a symbol) but still not unique.
             ;; We can capture a match
             ((setq complete-not-uniq
                    (semanticdb-find-tags-by-name
                     prefix
                     answer))
              (oset obj current-exact-match
                    complete-not-uniq)
              prefix)
             ;; Non unique match, return the string that handles
             ;; completion
             (t (or completion prefix)))))))

;; Redefined to use semanticdb-find-result-nth instead of
;; semanticdb-find-result-nth-in-buffer which caused unwanted
;; buffer switching.
;; Added support for unknown input when custom/semantic/complete-accepts-unknown
;; is set to true.
(defun semantic-complete-current-match ()
  "Calculate a match from the current completion environment.
Save this in our completion variable.  Make sure that variable
is cleared if any other keypress is made.
Return value can be:
  tag - a single tag that has been matched.
  string - a message to show in the minibuffer."
  ;; Query the environment for an active completion.
  (let ((collector semantic-completion-collector-engine)
        (displayor semantic-completion-display-engine)
        (contents (semantic-completion-text))
        matchlist
        answer)
    (if (string= contents "")
        ;; The user wants the defaults!
        (setq answer semantic-complete-active-default)
      ;; This forces a full calculation of completion on CR.
      (when (not custom/semantic/complete-accepts-unknown)
        (save-excursion
          (semantic-collector-calculate-completions collector contents nil))
        (semantic-complete-try-completion))
      (cond
       (custom/semantic/complete-accepts-unknown
        (setq answer (semantic-completion-text)))
       ;; Input match displayor focus entry
       ((setq answer (semantic-displayor-current-focus displayor))
        ;; We have answer, continue
        )
       ;; One match from the collector
       ((setq matchlist (semantic-collector-current-exact-match collector))
        (if (= (semanticdb-find-result-length matchlist) 1)
            (setq answer (car (semanticdb-find-result-nth matchlist 0)))
          (if (semantic-displayor-focus-abstract-child-p displayor)
              ;; For focusing displayors, we can claim this is
              ;; not unique.  Multiple focuses can choose the correct
              ;; one.
              (setq answer "Not Unique")
            ;; If we don't have a focusing displayor, we need to do something
            ;; graceful.  First, see if all the matches have the same name.
            (let ((allsame t)
                  (firstname (semantic-tag-name
                              (car
                               (semanticdb-find-result-nth matchlist 0))))
                  (cnt 1)
                  (max (semanticdb-find-result-length matchlist)))
              (while (and allsame (< cnt max))
                (if (not (string=
                          firstname
                          (semantic-tag-name
                           (car
                            (semanticdb-find-result-nth matchlist cnt)))))
                    (setq allsame nil))
                (setq cnt (1+ cnt)))
              ;; Now we know if they are all the same.  If they are, just
              ;; accept the first, otherwise complain.
              (if allsame
                  (setq answer (car (semanticdb-find-result-nth matchlist 0)))
                (setq answer "Not Unique"))))))
       ;; No match
       (t (setq answer "No Match"))))
    ;; Set it into our completion target.
    (when (or (semantic-tag-p answer)
              custom/semantic/complete-accepts-unknown)
      (setq semantic-complete-current-matched-tag answer)
      ;; Make sure it is up to date by clearing it if the user dares
      ;; to touch the keyboard.
      (add-hook 'pre-command-hook
                (lambda () (setq semantic-complete-current-matched-tag nil))))
    ;; Return it
    answer))

;; Redefined to use semanticdb-find-result-nth instead of
;; semanticdb-find-result-nth-in-buffer which caused unwanted
;; buffer switching.
(defun semantic-complete-default-to-tag (default)
  "Convert a calculated or passed in DEFAULT into a tag."
  (if (semantic-tag-p default)
      ;; Just return what was passed in.
      (setq semantic-complete-active-default default)
    ;; If none was passed in, guess.
    (if (null default)
        (setq default (semantic-ctxt-current-thing)))
    (if (null default)
        ;; Do nothing
        nil
      ;; Turn default into something useful.
      (let ((str
             (cond
              ;; Semantic-ctxt-current-symbol will return a list of
              ;; strings.  Technically, we should use the analyzer to
              ;; fully extract what we need, but for now, just grab the
              ;; first string
              ((and (listp default) (stringp (car default)))
               (car default))
              ((stringp default)
               default)
              ((symbolp default)
               (symbol-name default))
              (t
               (signal 'wrong-type-argument
                       (list default 'semantic-tag-p)))))
            (tag nil))
        ;; Now that we have that symbol string, look it up using the active
        ;; collector.  If we get a match, use it.
        (save-excursion
          (semantic-collector-calculate-completions
           semantic-completion-collector-engine
           str nil))
        ;; Do we have the perfect match???
        (let ((ml (semantic-collector-current-exact-match
                   semantic-completion-collector-engine)))
          (when ml
            ;; We don't care about uniqueness.  Just guess for convenience
            (setq tag (car (semanticdb-find-result-nth ml 0)))))
        ;; save it
        (setq semantic-complete-active-default tag)
        ;; Return it.. .whatever it may be
        tag))))

;; Replaced semantic-complete-done for custom/semantic-complete-done
;; in order to support raw string outputs of names not known to semantic.
(defvar custom/semantic-complete-key-map
  (let ((km (make-sparse-keymap)))
    (define-key km " " 'semantic-complete-complete-space)
    (define-key km "\t" 'semantic-complete-complete-tab)
    (define-key km "\C-m" 'custom/semantic-complete-done)
    (define-key km "\C-g" 'abort-recursive-edit)
    (define-key km "\M-n" 'next-history-element)
    (define-key km "\M-p" 'previous-history-element)
    (define-key km "\C-n" 'next-history-element)
    (define-key km "\C-p" 'previous-history-element)
    km)
  "Keymap used while completing across a list of tags.")

;; Accepts string result. Old behavior is to treat it as an error
;; message. Result from original implementation is always tag, not raw
;; unresolved name.
(setq custom/semantic/complete-accepts-unknown nil)
(defun custom/semantic-complete-done ()
  "Accept the current input."
  (interactive)
  (let* ((custom/semantic/complete-accepts-unknown t)
         (ans (semantic-complete-current-match)))
    (exit-minibuffer)))

;; Replaced semantic-complete-key-map for custom/semantic-complete-key-map
;; in order to support raw string outputs.
(defun custom/semantic-complete-read-tag-engine
    (collector displayor prompt default-tag initial-input history)
  "Read a semantic tag, and return a tag for the selection.
Argument COLLECTOR is an object which can be used to calculate
a list of possible hits.  See `semantic-completion-collector-engine'
for details on COLLECTOR.
Argument DISPLAYOR is an object used to display a list of possible
completions for a given prefix.  See`semantic-completion-display-engine'
for details on DISPLAYOR.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to story the history in."
  (let* ((semantic-completion-collector-engine collector)
         (semantic-completion-display-engine displayor)
         (semantic-complete-active-default nil)
         (semantic-complete-current-matched-tag nil)
         (default-as-tag (semantic-complete-default-to-tag default-tag))
         (default-as-string (when (semantic-tag-p default-as-tag)
                              (semantic-tag-name default-as-tag))))
    (when default-as-string
      ;; Add this to the prompt.
      ;;
      ;; I really want to add a lookup of the symbol in those
      ;; tags available to the collector and only add it if it
      ;; is available as a possibility, but I'm too lazy right
      ;; now.
      ;;
      ;; @todo - move from () to into the editable area
      (if (string-match ":" prompt)
          (setq prompt (concat
                        (substring prompt 0 (match-beginning 0))
                        " (default " default-as-string ")"
                        (substring prompt (match-beginning 0))))
        (setq prompt (concat prompt " (" default-as-string "): "))))
    ;; Perform the Completion
    (unwind-protect
        (read-from-minibuffer prompt
                              initial-input
                              custom/semantic-complete-key-map
                              nil
                              (or history
                                  'semantic-completion-default-history)
                              default-tag)
      (semantic-collector-cleanup semantic-completion-collector-engine)
      (semantic-displayor-cleanup semantic-completion-display-engine))
    ;; Extract the tag from the completion machinery.
    semantic-complete-current-matched-tag))

;; Redefined because original used more sophisticated displayor which
;; refused to select not unique tag name.
(defun custom/semantic-complete-read-tag-project (prompt &optional
                                                         default-tag
                                                         initial-input
                                                         history)
  "Ask for a tag by name from the current project.
Available tags are from the current project, at the top level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in."
  ;; Load all existing databases for current project and its
  ;; dependencies into memory, otherwise this function will be unable
  ;; to provide autocompletion for all project symbols. Of course,
  ;; unparsed files which are logically missing in DB are not loaded
  ;; and symbols from them are invisible for autocompletion.
  (save-excursion
    (custom/semantic/load-all-project-dbs)
    (custom/semantic-complete-read-tag-engine
     ;; I used semantic-collector-project-brutish in past but my new
     ;; collector is able to complete even dot chains.
     (custom/semantic/collector-project-chained prompt
                                                :buffer (current-buffer)
                                                :path (current-buffer))
     (semantic-displayor-traditional)
     prompt
     default-tag
     initial-input
     history)))

;;;; GOTO PARENT
;; CODE:

(setq custom/semantic/select-best-tag-by-user nil)

(defmacro custom/semantic/best-tag-user-assist-enabled (method &rest forms)
  (declare (indent 2) (debug t))
  `(let ((custom/semantic/select-best-tag-by-user ',method))
     ,@forms))

(defmacro custom/semantic/best-tag-user-assist-disabled (&rest forms)
  `(let ((custom/semantic/select-best-tag-by-user nil))
     ,@forms))

;; Added support for optional user intervention when choosing best tag.
(defun semantic-analyze-select-best-tag (sequence &optional tagclass)
  "For a SEQUENCE of tags, all with good names, pick the best one.
If SEQUENCE is made up of namespaces, merge the namespaces together.
If SEQUENCE has several prototypes, find the non-prototype.
If SEQUENCE has some items w/ no type information, find the one with a type.
If SEQUENCE is all prototypes, or has no prototypes, get the first one.
Optional TAGCLASS indicates to restrict the return to only
tags of TAGCLASS."
  ;; If there is a srew up and we get just one tag.. massage over it.
  (when (semantic-tag-p sequence)
    (setq sequence (list sequence)))
  ;; Filter out anything not of TAGCLASS
  (when tagclass
    (setq sequence
          (semantic-find-tags-by-class tagclass
                                       sequence)))
  (if (< (length sequence) 2)
      ;; If the remaining sequence is 1 tag or less, just return it
      ;; and skip the rest of this mumbo-jumbo.
      (car sequence)
    ;; 1)
    ;; This step will eliminate a vast majority of the types,
    ;; in addition to merging namespaces together.
    ;;
    ;; 2)
    ;; It will also remove prototypes.
    (require 'semantic/db-typecache)
    (setq sequence (semanticdb-typecache-merge-streams sequence nil))
    (if (< (length sequence) 2)
        ;; If the remaining sequence after the merge is 1 tag or less,
        ;; just return it and skip the rest of this mumbo-jumbo.
        (car sequence)
      (let ((best nil)
            (notypeinfo nil))
        (if custom/semantic/select-best-tag-by-user
            (cond
             ((equal custom/semantic/select-best-tag-by-user
                     'user)
              (setq best (custom/semantic/choose-tag sequence)))
             ((equal custom/semantic/select-best-tag-by-user
                     'type+user)
              (setq best (semantic-find-tags-by-class 'type sequence))
              (if (> (length best) 1)
                  (setq best (custom/semantic/choose-tag best))
                (setq best (car-safe best))))
             (t (error "Unknown selector %s"
                       custom/semantic/select-best-tag-by-user)))
          (while (and (not best) sequence)
            ;; 3) select a non-prototype.
            (if (not (semantic-tag-type (car sequence)))
                (setq notypeinfo (car sequence))
              (setq best (car sequence)))
            (setq sequence (cdr sequence))))
        ;; Select the best, or at least the prototype.
        (or best notypeinfo)))))

(defun senator-go-to-up-reference (&optional tag)
  "Move up one reference from the current TAG.
A \"reference\" could be any interesting feature of TAG.
In C++, a function may have a `parent' which is non-local.
If that parent which is only a reference in the function tag
is found, we can jump to it.
Some tags such as includes have other reference features."
  (interactive)
  (semantic-error-if-unparsed)
  (let ((result (semantic-up-reference (or tag (semantic-current-tag)))))
    (if (not result)
        (error "No up reference found!")
      (push-mark)
      (cond
       ;; A tag
       ((semantic-tag-p result)
        (custom/semantic/goto-tag result))
       ;; Buffers
       ((bufferp result)
        (xref-push-marker-stack)
        (pop-to-buffer-same-window result))
       ;; Files
       ((and (stringp result) (file-exists-p result))
        (xref-push-marker-stack)
        (find-file result))
       (t
        (error "Unknown result type from `semantic-up-reference'"))))))

(defun semantic-up-reference-default (tag)
  "Return a tag that is referred to by TAG.
Makes C/C++ language like assumptions."
  (custom/semantic/with-disabled-grep-db
      (cond ((semantic-tag-faux-p tag)
             ;; Faux tags should have a real tag in some other location.
             (require 'semantic/sort)
             (let ((options (semantic-tag-external-class tag)))
               (custom/semantic/choose-tag options)))

            ;; Include always point to another file.
            ((eq (semantic-tag-class tag) 'include)
             (let ((file (semantic-dependency-tag-file tag)))
               (cond
                ((or (not file) (not (file-exists-p file)))
                 (error "Could not locate include %s"
                        (semantic-tag-name tag)))
                ((get-file-buffer file)
                 (get-file-buffer file))
                ((stringp file)
                 file))))

            ;; Is there a parent of the function to jump to?
            ((and  (semantic-tag-of-class-p tag 'function)
                   (not (semantic-tag-get-attribute tag :prototype-flag)))
             (let* ((scope (semantic-calculate-scope (point))))
               (custom/semantic/choose-tag (oref scope parents))))

            ;; Is there a non-prototype version of the tag to jump to?
            ((semantic-tag-get-attribute tag :prototype-flag)
             (require 'semantic/analyze/refs)
             (custom/semantic/with-enabled-grep-db
                 (let* ((sar (semantic-analyze-tag-references tag))
                        (impls (semantic-analyze-refs-impl sar t)))
                   (custom/semantic/choose-tag impls))))

            ;; If this is a datatype, and we have superclasses.
            ((and (semantic-tag-of-class-p tag 'type)
                  (semantic-tag-type-superclasses tag))
             (require 'semantic/analyze)
             (let* ((scope (semantic-calculate-scope (point)))
                    (parents (semantic-tag-type-superclasses tag))
                    (parent (if (<= (length parents) 1)
                                (car-safe parents)
                              (ido-completing-read "Choose parent: "
                                                   parents))))
               (or (custom/semantic/best-tag-user-assist-enabled type+user
                       (semantic-analyze-find-tag parent 'type scope))
                   (if (y-or-n-p (format (concat "Failed to find '%s' "
                                                 "intelligently, try "
                                                 "brute force?")
                                         parent))
                       (custom/semantic/with-enabled-grep-db
                           (let* ((res (semantic-symref-find-tags-by-name
                                        parent))
                                  (tags
                                   (if res
                                       (semantic-symref-result-get-tags-as-is
                                        res))))
                             (setq tags (semantic-find-tags-by-class 'type
                                                                     tags))
                             (if tags (custom/semantic/choose-tag tags))))))))

            ;; Get the data type, and try to find that.
            ((semantic-tag-type tag)
             (require 'semantic/analyze)
             (let ((scope (semantic-calculate-scope (point))))
               (semantic-analyze-tag-type tag scope)))

            (t nil))))

;; TODO: Implement simple semantic aware font locking.

;; TODO: Implement call graph using https://github.com/DamienCassou/hierarchy.
;; For navigation in it use https://www.emacswiki.org/emacs/TreeMode.

;; TODO: Create my own EDE project class which will get all information from
;; JSON config and will be created automatically upon finding that
;; config somewhere in lower levels of file tree.
;;
;; For that instantiation, use either something build-in in EDE or
;; just hook yourself to semantic-default-c-setup.
;;
;; ede-cpp-root-project-list is a list of all registered projects

;; TODO: Try to download and compile newest parser definition from
;; https://sourceforge.net/p/cedet/git/ci/master/tree/lisp/cedet/semantic/bovine/c.by
;; It might fix some things and provide speedup.
;;
;; Python: Some issue with general parsing. Probably issue with tabs
;; vs spaces.
;;
;; C/C++:
;;    // Reason is that newline in brackets.
;;    int unparseable1[8
;;                     + 9] = {0};
;;
;;    // Reason is that sizeof() invocation.
;;    // Parser fails here probably.
;;    uint32_t unparseable3 = sizeof(uint8_t);

;; NOTE: Interesting functions where include resolve process occurs
;;   semantic-dependency-tag-file
;;   semantic-dependency-find-file-on-path

;; Overrided because default implementation completely ignored
;; function parameters in function signature.
(defun semantic-symref-hits-in-region (target hookfcn start end)
  "Find all occurrences of the symbol TARGET that match TARGET the tag.
For each match, call HOOKFCN.
HOOKFCN takes three arguments that match
`semantic-analyze-current-symbol's use of HOOKFCN.
  ( START END PREFIX )

Search occurs in the current buffer between START and END."
  (require 'semantic/idle)
  (save-excursion
    (goto-char start)
    (let* ((str (semantic-tag-name target))
           (case-fold-search semantic-case-fold)
           (regexp (concat "\\<" (regexp-quote str) "\\>")))
      (while (re-search-forward regexp end t)
        (when (semantic-idle-summary-useful-context-p)
          (semantic-analyze-current-symbol
           (lambda (start end prefix)
             (let ((tag (car (nreverse prefix)))
                   (parent-tag (semantic-current-tag-parent)))
               ;; check for semantic match on the text match.
               (when (or (and (semantic-tag-p tag)
                              (semantic-equivalent-tag-p target tag))
                         ;; Hack in order to get recognized functional
                         ;; arguments.
                         ;; When tag failed to be analyzed, it might
                         ;; actually be hit into function parameter
                         ;; enumeration.
                         ;; In such case, we must somehow decide if it
                         ;; is true.
                         ;; 1) We get parent tag.
                         ;; 2) If it is function, we get all its arguments.
                         ;; 3) We try to find all arguments which
                         ;;    match to unresolved prefix name.
                         ;; 4) After that, we iterate over result from
                         ;;    step 3 and try to find at least one similar
                         ;;    parameter tag to the passed target tag.
                         ;;
                         ;; If at least one hit was found, we can be
                         ;; pretty sure that we are looking at
                         ;; functional parameter.
                         (and (stringp tag)
                              (semantic-tag-p parent-tag)
                              (equal (semantic-tag-class parent-tag)
                                     'function)
                              (loop for candidate
                                    in (semantic-find-tags-by-name
                                        tag
                                        (semantic-tag-function-arguments
                                         parent-tag))
                                    if (semantic-equivalent-tag-p target
                                                                  candidate)
                                    collect candidate)))
                 (save-excursion (funcall hookfcn
                                          start
                                          end
                                          prefix)))))
           (point)))))))

;; Overrided because default implementation ignored function
;; parameters in function signatures.
(defun semantic-symref-rename-local-variable ()
  "Fancy way to rename the local variable under point.
Depends on the SRecode Field editing API."
  (interactive)
  ;; Do the replacement as needed.
  (let* ((ctxt (semantic-analyze-current-context))
         (target (car (reverse (oref ctxt prefix))))
         (tag (semantic-current-tag)))
    ;; Hack for the situation when we are trying to renamed function
    ;; parameter and point is positioned in function signature.
    ;; We dont get any analysis here, so we must try to do it ourselves.
    (when (and (stringp target)
               (semantic-tag-with-position-p tag)
               (equal (semantic-tag-name tag) target)
               (semantic-current-tag-parent)
               (equal (semantic-tag-class (semantic-current-tag-parent))
                      'function))
      (setq target tag
            tag (semantic-current-tag-parent)))
    (when (not tag)
      (error "Failed to find containing tag!"))
    (when (or (not target)
              (not (semantic-tag-with-position-p target)))
      (error "Cannot identify symbol under point"))
    (when (not (semantic-tag-of-class-p target 'variable))
      (error "Can only rename variables"))
    (when (or (< (semantic-tag-start target) (semantic-tag-start tag))
              (> (semantic-tag-end target) (semantic-tag-end tag)))
      (error "Can only rename variables declared in %s"
             (semantic-tag-name tag)))
    ;; I think we're good for this example.  Give it a go through
    ;; our fancy interface from SRecode.
    (require 'srecode/fields)
    ;; Make sure there is nothing active.
    (let ((ar (srecode-active-template-region)))
      (when ar (srecode-delete ar)))
    (let ((srecode-field-archive nil)
          (region nil))
      (semantic-symref-hits-in-region
       target (lambda (start end prefix)
                ;; For every valid hit, create one field.
                (srecode-field "LOCAL" :name "LOCAL" :start start :end end))
       (semantic-tag-start tag) (semantic-tag-end tag))
      ;; Now that the fields are setup, create the region.
      (setq region (srecode-template-inserted-region
                    "REGION" :start (semantic-tag-start tag)
                    :end (semantic-tag-end tag)))
      ;; Activate the region.
      (if (not (null (oref region fields)))
          (srecode-overlaid-activate region)
        (error "No reference in region was found!")))))

;; Overrided because old implementation does not detected invalid
;; tagname hits. That way, users blamed CEDET instead of shitty tools
;; which CEDET uses (hello, GNU Global).
(defun semantic-symref-hit-to-tag-via-buffer
    (hit searchtxt searchtype &optional open-buffers)
  "Convert the symref HIT into a TAG by looking up the tag via a buffer.
Return the Semantic tag associated with HIT.
SEARCHTXT is the text that is being searched for.
Used to narrow the in-buffer search.
SEARCHTYPE is the type of search (such as 'symbol or 'tagname).
Optional OPEN-BUFFERS, when nil will use a faster version of
`find-file' when a file needs to be opened.  If non-nil, then
normal buffer initialization will be used.
This function will leave buffers loaded from a file open, but
will add buffers that must be opened to `semantic-symref-recently-opened-buffers'.
Any caller MUST deal with that variable, either clearing it, or deleting the
buffers that were opened."
  (let* ((line (car hit))
         (file (cdr hit))
         (buff (find-buffer-visiting file))
         (tag nil))
    (cond
     ;; We have a buffer already.  Check it out.
     (buff
      (set-buffer buff))

     ;; We have a table, but it needs a refresh.
     ;; This means we should load in that buffer.
     (t
      (let ((kbuff
             (if open-buffers
                 ;; Even if we keep the buffers open, don't
                 ;; let EDE ask lots of questions.
                 (let ((ede-auto-add-method 'never))
                   (find-file-noselect file t))
               ;; When not keeping the buffers open, then
               ;; don't setup all the fancy froo-froo features
               ;; either.
               (semantic-find-file-noselect file t))))
        (set-buffer kbuff)
        (push kbuff semantic-symref-recently-opened-buffers)
        (semantic-fetch-tags))))

    ;; Too much baggage in goto-line
    ;; (goto-line line)
    (goto-char (point-min))
    (forward-line (1- line))

    ;; Search forward for the matching text.
    ;; FIXME: This still fails if the regexp uses something specific
    ;; to the extended syntax, like grouping.
    (when (re-search-forward (if (memq searchtype '(regexp tagregexp))
                                 searchtxt
                               (regexp-quote searchtxt))
                             (point-at-eol)
                             t)
      (goto-char (match-beginning 0)))

    (setq tag (semantic-current-tag))

    ;; If we are searching for a tag, but bound the tag we are looking
    ;; for, see if it resides in some other parent tag.
    ;;
    ;; If there is no parent tag, then we still need to hang the originator
    ;; in our list.
    (when (and (eq searchtype 'symbol)
               (string= (semantic-tag-name tag) searchtxt))
      (setq tag (or (semantic-current-tag-parent) tag)))

    (when (and (eq searchtype 'tagname)
               (not (string= (semantic-tag-name tag)
                             searchtxt))
               (not (string= (semantic-tag-name
                              ;; Hack for potential nil return value.
                              ;; Dont want to create special var for
                              ;; it and dont want to call it twice.
                              (or (semantic-current-tag-parent)
                                  tag))
                             searchtxt)))
      (let ((msg (format (concat "Hit %s:%s does not match to %s "
                                 "(searched for %s), this is with high "
                                 "probability error of symref backend, not "
                                 "semantic!")
                         file
                         line
                         (semantic-tag-name tag)
                         searchtxt)))
        (message msg)
        (setq tag nil)))

    ;; Copy the tag, which adds a :filename property.
    (when tag
      (setq tag (semantic-tag-copy tag nil t))
      ;; Ad this hit to the tag.
      (semantic--tag-put-property tag :hit (list line)))
    tag))

;; Walks all for cycles in range and parses variables defined in them.
;;
;; WARNING: Does not respect for cycle scope nesting.
;;
;; NOTE: When reworking current hack to correct handling of cycle scopes, dont
;; forget that some for cycles may by without scope braces.
;;
;; Following functions will be very handy when dealing with cycle
;; context analysis:
;;     semantic-beginning-of-context
;;     semantic-end-of-context
(defun custom/semantic/parse-vars-in-fors-in-c/c++ (context-begin context-end)
  (save-excursion
    (goto-char context-begin)
    (let ((vars nil))
      (while (re-search-forward (pcre-to-elisp/cached "^\\s*for\\s*\\(")
                                context-end
                                t)
        (goto-char (match-end 0))
        (push (semantic-parse-region (point)
                                     (save-excursion
                                       (semantic-end-of-context)
                                       (point))
                                     'bovine-inner-scope
                                     nil
                                     t)
              vars)
        (semantic-end-of-context))
      ;; Reverse the tags so that first tag is the most far away one.
      ;; This is important for handling variable shadowing.
      (setq vars (nreverse (apply #'append vars)))
      ;; And here we have implemented variable shadowing using hash map.
      (let ((map (@dict-new)))
        (loop for var in vars
              for varname = (semantic-tag-name var)
              do (@dict-set varname var map))
        (setq vars nil)
        (@map (lambda (key val) (push val vars)) map))
      vars)))

;; Enhanced version of function semantic-get-local-variables-default.
;; In C/C++, local variables in for cycles are not parsed because
;; variable parser just refuses to parse stuff inside statements.
;; As an workaround, we walk all seen for cycles and release semantic
;; variable parser only on its internals which are parsed just fine.
;;
;; WARNING: This hack does not respect nesting of cycle contextes, it
;;          will just parse all variables in for cycles located in
;;          <start-of-toplevel-context,starting-point>. As an result, more
;;          variables is catched even though they are in inaccessible
;;          scopes.
;;
;; NOTE: It is not exactly true that variable parser refuses to parse
;;       variables in cycles. Actually it is able to parse them when lexer
;;       is configured with depth of 2 instead of nil which defaults to 0.
;;       Unfortunately it is a lot slower and it catches more false
;;       positives than when configured with lexer depth 0 and it is even worse
;;       when it comes to respecting nesting of for cycles. At least, my
;;       hack knows that all for cycles after cursor are out of game.
;;
;; TODO: Add support for semantically correct parsing of for cycles.
;;       Parse all parent for cycles and skip for cycles in neighbor
;;       syntactical branches.
(defun custom/semantic/semantic-get-local-variables-in-c/c++ ()
  "Get local values from a specific context.
Uses the bovinator with the special top-symbol `bovine-inner-scope'
to collect tags, such as local variables or prototypes."
  ;; This assumes a bovine parser.  Make sure we don't do
  ;; anything in that case.
  (when (and semantic--parse-table (not (eq semantic--parse-table t)))
    (let ((vars (semantic-get-cache-data 'get-local-variables)))
      (if vars
          (progn
            ;;(message "Found cached vars.")
            vars)
        (let ((vars2 nil)
              ;; We want nothing to do with funny syntaxing while doing this.
              (semantic-unmatched-syntax-hook nil)
              (start (point))
              (firstusefulstart nil)
              (last-context-begin nil))
          (while (not (semantic-up-context (point) 'function))
            (when (not vars)
              (setq firstusefulstart (point)))
            (setq last-context-begin (point))
            (save-excursion
              (forward-char 1)
              (setq vars
                    ;; Note to self: semantic-parse-region returns cooked
                    ;; but unlinked tags.  File information is lost here
                    ;; and is added next.
                    (append (semantic-parse-region (point)
                                                   (save-excursion
                                                     (semantic-end-of-context)
                                                     (point))
                                                   'bovine-inner-scope
                                                   nil
                                                   t)
                            vars))))
          ;; Get variables from all previous for cycles.
          (when last-context-begin
            (setq vars
                  (append
                   (custom/semantic/parse-vars-in-fors-in-c/c++
                    last-context-begin
                    start)
                   vars)))
          ;; Modify the tags in place.
          (setq vars2 vars)
          (while vars2
            (semantic--tag-put-property (car vars2)
                                        :filename (buffer-file-name))
            (setq vars2 (cdr vars2)))
          ;; Hash our value into the first context that produced useful results.
          (when (and vars firstusefulstart)
            (let ((end (save-excursion
                         (goto-char firstusefulstart)
                         (save-excursion
                           (unless (semantic-end-of-context)
                             (point))))))
              ;;(message "Caching values %d->%d." firstusefulstart end)
              (semantic-cache-data-to-buffer (current-buffer)
                                             firstusefulstart
                                             (or end
                                                 ;; If the end-of-context fails,
                                                 ;; just use our cursor starting
                                                 ;; position.
                                                 start)
                                             vars
                                             'get-local-variables
                                             'exit-cache-zone)))
          ;; Return our list.
          vars)))))

;; Override this function for c mode only. We dont want this logic
;; anywhere else.
(define-mode-local-override semantic-get-local-variables c-mode
  (&optional point)
  "Get all variables from cycles in c mode"
  (custom/semantic/semantic-get-local-variables-in-c/c++))

;; Override this function for c++ mode only. We dont want this logic
;; anywhere else.
;;
;; Borrowed injecting of this variable from old override.
(define-mode-local-override semantic-get-local-variables c++-mode
  (&optional point)
  "Get all variables from cycles in c++ mode"
  (let* ((origvar (custom/semantic/semantic-get-local-variables-in-c/c++))
         (ct (semantic-current-tag))
         (p (when (semantic-tag-of-class-p ct 'function)
              (or (semantic-tag-function-parent ct)
                  (car-safe (semantic-find-tags-by-type
                             "class" (semantic-find-tag-by-overlay)))))))
    ;; If we have a function parent, then that implies we can
    (if p
        ;; Append a new tag THIS into our space.
        (cons (semantic-tag-new-variable "this" p nil :pointer 1)
              origvar)
      ;; No parent, just return the usual.
      origvar)))

;; NOTE: semantic-get-local-variables for emacs lisp omit position
;; information. Thats funcking bad. For what it is if it cant jump to
;; local variable? Having variable name is good for nothing.
