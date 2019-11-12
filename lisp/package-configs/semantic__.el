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

;; Replaced by manual triggering (M-g).
;;
;; (advice-add 'save-buffer :after
;;             (lambda (&rest args)
;;               (if (or (equal major-mode 'c-mode)
;;                       (equal major-mode 'c++-mode)
;;                       (equal major-mode 'emacs-lisp-mode))
;;                   (save-mark-and-excursion
;;                     (semantic-force-refresh)))))

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

(defun semantic-ia-fast-jump (point)
  "Jump to the tag referred to by the code at POINT.
Uses `semantic-analyze-current-context' output to identify an accurate
origin of the code at point."
  (interactive "d")
  ;; Reparse buffer when needed.
  (semantic-fetch-tags)
  (custom/semantic/load-all-project-dbs)
  (custom/semantic/with-disabled-grep-db
   (let* ((ctxt (semantic-analyze-current-context point))
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
  (let* ((tag (semantic-current-tag))
         (sar (if tag
                  (semantic-analyze-tag-references tag)
                (error "Point must be in a declaration")))
         (target (if (semantic-tag-prototype-p tag)
                     (car (semantic-analyze-refs-impl sar t))
                   (car (semantic-analyze-refs-proto sar t)))))
    (when (not target)
      (error "Could not find suitable %s"
             (if (semantic-tag-prototype-p tag)
                 "implementation"
               "prototype")))
    (custom/semantic/goto-tag target)))

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
  (interactive (list (custom/semantic/with-disabled-grep-db
                      (custom/semantic-complete-read-tag-project
                       "Look for symbol: "
                       nil
                       (thing-at-point 'symbol)))))
  ;; Use modified symref module for getting all tags with target name in
  ;; current project and all its dependencies.
  ;; Bypasses bug when brute deep searching all tables in project
  ;; using standard semantic find routines.
  (custom/semantic/with-enabled-grep-db
   (let ((tags (let ((res (semantic-symref-find-tags-by-name sym)))
                 (if res
                     (semantic-symref-result-get-tags-as-is res)))))
     (if tags
         (let* ((chosen-tag (custom/semantic/choose-tag tags)))
           (custom/semantic/goto-tag chosen-tag))
       (message "No tags found for %s" sym)))))

(defvar-local *is-prefix-pointer-state* (cons nil 'allow))
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

(defun custom/semantic/get-local-tags-no-includes (file symbol)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (let ((tags (custom/semantic/flatten-tags-table
                   (semantic-fetch-tags))))
        (loop for tag in tags
              if (equal symbol (semantic-tag-name tag))
              collect tag)))))

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
                               (limit-len 67))
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
  (xref-push-marker-stack)
  (find-file (buffer-file-name (semantic-tag-buffer tag)))
  (semantic-go-to-tag tag)
  (if (not semantic-idle-scheduler-mode)
      (semantic-idle-scheduler-mode))
  (recenter)
  (pulse-momentary-highlight-region (semantic-tag-start tag)
                                    (semantic-tag-end tag)))

;;;; INDEX MANAGEMENT CODE
;; CODE:

(defun custom/semantic/index-directory (root &optional selection-regex)
  (interactive (list (ido-read-directory-name "Select directory: ")))
  (if (not selection-regex)
      (setq selection-regex
            (pcre-to-elisp/cached ".*\\.(?:c|cpp|h|hpp|cxx|hxx)$")))
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
          (when (string-match-p selection-regex file)
            (ignore-errors (semanticdb-file-table-object file)))
        (progn (semanticdb-save-all-db)
               (custom/semantic/index-directory file selection-regex))))))

(setq custom/semantic/loaded-projects (@dict-new))
(defun custom/semantic/load-all-project-dbs ()
  (interactive)
  (when (not (@at custom/semantic/loaded-projects
                  (semantic-symref-calculate-rootdir)))
    (let* ((proj-roots (cons (semantic-symref-calculate-rootdir)
                             (custom/ede/get-project-dependencies)))
           (store-path (file-name-as-directory semanticdb-default-save-directory))
           (check-regexp (concat "^\\(?:"
                                 (s-join "\\|"
                                         (loop for proj-root in proj-roots
                                               collect (regexp-quote proj-root)))
                                 "\\)"))

           (raw-cache-paths (directory-files store-path))
           (cache-paths (loop for raw-cache in raw-cache-paths
                              for cache = (cedet-file-name-to-directory-name
                                           raw-cache)
                              if (s-match check-regexp cache)
                              collect raw-cache)))
      (loop for cache in cache-paths
            for full-path = (expand-file-name (concat store-path cache))
            if (not (semanticdb-file-loaded-p full-path))
            do (progn (message "Loading: %s" full-path)
                      (let* ((db (semanticdb-load-database full-path))
                             (tables (ignore-errors
                                       (semanticdb-get-database-tables db))))
                        (oset db
                              reference-directory
                              (s-replace-regexp "semantic.cache"
                                                ""
                                                (cedet-file-name-to-directory-name
                                                 cache)))
                        (loop for table in tables
                              if (and (file-exists-p
                                       (concat (oref db reference-directory)
                                               (oref table file)))
                                      (semanticdb-needs-refresh-p table))
                              do (semanticdb-refresh-table table t))))))
    (@dict-set (semantic-symref-calculate-rootdir)
               t
               custom/semantic/loaded-projects)))

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
                                     (s-replace-regexp "\\\\" "/" hit-path)
                                     hit-line
                                     (s-replace-regexp "\\\\s*$" "" hit-payload)
                                     "\n")))))))))

(defun custom/filter-raw-grep-output (hits searchfor)
  (setq custom/TAGNAME-HITS hits)
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
    (setq custom/TAGNAME-RESULT result)
    result))

(setq custom/semantic/grep-db-enabled t)

(defmacro custom/semantic/with-disabled-grep-db (&rest forms)
  `(let ((custom/semantic/grep-db-enabled nil))
     ,@forms))

(defmacro custom/semantic/with-enabled-grep-db (&rest forms)
  `(let ((custom/semantic/grep-db-enabled t))
     ,@forms))

(cl-defmethod semantic-symref-perform-search ((tool semantic-symref-tool-grep))
  "Perform a search with Grep."
  (when custom/semantic/grep-db-enabled
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

;; Fixed invalid usage of semantic-analyze-current-context which
;; happened to be outside of semantic buffer.
;; Also it was wrapped in error catcher because abstract class has no
;; context slot.
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
                      (when (slot-boundp obj 'first-pass-completions)
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
    (setq completion (try-completion
                      prefix
                      (semanticdb-strip-find-results answer)))
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
  (custom/semantic/load-all-project-dbs)
  (custom/semantic-complete-read-tag-engine
   (semantic-collector-project-brutish prompt
                                       :buffer (current-buffer)
                                       :path (current-buffer))
   (semantic-displayor-traditional)
   prompt
   default-tag
   initial-input
   history))

;;;; GOTO PARENT
;; CODE:

(setq custom/semantic/select-best-tag-by-user nil)

(defmacro custom/semantic/best-tag-user-assist-enabled (&rest forms)
  `(let ((custom/semantic/select-best-tag-by-user t))
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
            (custom/semantic/choose-tag sequence)
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
         ((semantic-tag-of-class-p tag 'function)
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
            (or (custom/semantic/best-tag-user-assist-enabled
                 (semantic-analyze-find-tag parent 'type scope))
                (if (y-or-n-p (format (concat "Failed to find '%s' "
                                              "intelligently, try "
                                              "brute force?")
                                      parent))
                    (custom/semantic/with-enabled-grep-db
                     (let* ((res (semantic-symref-find-tags-by-name parent))
                            (tags (if res (semantic-symref-result-get-tags-as-is
                                           res))))
                       (if tags (custom/semantic/choose-tag tags))))))))

         ;; Get the data type, and try to find that.
         ((semantic-tag-type tag)
          (require 'semantic/analyze)
          (let ((scope (semantic-calculate-scope (point))))
            (semantic-analyze-tag-type tag scope)))

         (t nil))))

;; TODO: symref fails to recognize symbols in macros
;; only macro name is include in its bounds
;; use regexp to deduce real macro bounds

;; TODO: Implement simple semantic aware font locking.

;; TODO: Implement call graph using https://github.com/DamienCassou/hierarchy.
;; For navigation in it use https://www.emacswiki.org/emacs/TreeMode.

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
