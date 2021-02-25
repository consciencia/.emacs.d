(require 'semantic/symref)
(require 'semantic/symref/grep)
(require 'semantic/db)
(require 'data-debug)

(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt))

;;;###autoload
(defun semanticdb-enable-grep-databases (&optional noerror)
  "Enable the use of the grep back end for all files in C/C++.
This will add an instance of a grep database to each buffer in a
grep supported hierarchy."
  (dolist (mode '(c-mode c++-mode))
    (let ((ih (mode-local-value mode 'semantic-init-mode-hook)))
      (eval `(setq-mode-local
              ,mode semantic-init-mode-hook
              (cons 'semanticdb-enable-grep-hook ih)))))
  t)

(defun semanticdb-enable-grep-hook ()
  "Add support for grep in the current buffer via `semantic-init-hook'."
  (semanticdb-enable-grep-in-buffer t))

(defclass semanticdb-project-database-grep
  ;; @todo - convert to one DB per directory.
  (semanticdb-project-database eieio-instance-tracker) ()
  "Database representing a grep virtual tags file.")

(defun semanticdb-enable-grep-in-buffer (&optional dont-err-if-not-available)
  "Enable a grep database in the current buffer.
When grep is not available for this directory, display a message
if optional DONT-ERR-IF-NOT-AVAILABLE is non-nil; else throw an error."
  (interactive "P")
  (if (file-exists-p (semantic-symref-calculate-rootdir))
      (setq
       ;; Add to the system database list.
       semanticdb-project-system-databases
       (cons (semanticdb-project-database-grep "Grep")
             semanticdb-project-system-databases)
       ;; Apply the throttle.
       semanticdb-find-default-throttle
       (append semanticdb-find-default-throttle '(omniscience)))
    (if dont-err-if-not-available
        nil ;; (message "No grep support in %s" default-directory)
      (error "No grep support in %s" default-directory))))

;;; Classes:
(defclass semanticdb-table-grep (semanticdb-search-results-table)
  ((major-mode :initform nil))
  "A table for returning search results from grep.")

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-grep)
                                       &optional buffer)
  "Return t, pretend that this table's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  ;; @todo - hack alert!
  t)

(defmethod object-print ((obj semanticdb-table-grep) &rest strings)
  "Pretty printer extension for `semanticdb-table-cscope'.
Adds the number of tags in this file to the object print name."
  (apply 'call-next-method obj (cons " (proxy)" strings)))

;;; Filename based methods
;;
(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-grep))
  "For a grep database, there are no explicit tables.
For each file hit, get the traditional semantic table from that file."
  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-grep "Grep Search Table")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)))
  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-grep)
                                  filename)
  "From OBJ, return FILENAME's associated table object."
  ;; We pass in "don't load".  I wonder if we need to avoid that or not?
  (car (semanticdb-get-database-tables obj)))

;;; Search Overrides
;;
;; Only NAME based searches work with CSCOPE as that is all it tracks.
;;
(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-grep) name &optional tags)
  "Find all tags named NAME in TABLE. Return a list of tags."
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (call-next-method)
    ;; Call out to grep for some results.
    (let* ((semantic-symref-tool 'grep)
           (result (semantic-symref-find-tags-by-name name 'project)))
      (when result
        ;; We could ask to keep the buffer open, but that annoys
        ;; people.
        (semantic-symref-result-get-tags result)))))

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-grep) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    (let* ((semantic-symref-tool 'grep)
           (result (semantic-symref-find-tags-by-regexp regex 'project)))
      (when result
        (semantic-symref-result-get-tags result)))))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-grep) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    (let* ((semantic-symref-tool 'grep)
           (result (semantic-symref-find-tags-by-completion prefix 'project))
           (faketags nil))
      (when result
        (dolist (T (oref result :hit-text))
          ;; We should look up each tag one at a time, but I'm lazy!
          ;; Doing this may be good enough.
          (setq faketags (cons
                          (semantic-tag T 'function :faux t)
                          faketags)))
        faketags))))

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above.
;;
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-grep) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for cscope."
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-grep) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for cscope."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-grep) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for cscope."
  (semanticdb-find-tags-for-completion-method table prefix tags))
