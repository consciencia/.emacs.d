;; Actual version has completely broken subtree feature so I'm using my
;; local snaphost instead of the version from MELPA.
;;
;; (custom/install-package-when-needed 'dired-imenu)
;; (custom/install-package-when-needed 'dired-subtree)
(custom/install-package-when-needed 'dash)
(require 'dired-aux)
(require 'dired-x)
(require 'wdired)
(require 'dired+)
(require 'dired-subtree)
(require 'dired-imenu)
(require 'xref)

(eval-after-load "dired-aux"
 '(add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(setq diredp-bind-problematic-terminal-keys nil
      wdired-allow-to-change-permissions t
      dired-recursive-copies 'top
      dired-recursive-deletes 'top
      dired-dwim-target t)
(diredp-toggle-find-file-reuse-dir 1)

(define-key dired-mode-map (kbd "<C-kp-subtract>")
  'diredp-up-directory-reuse-dir-buffer)
(define-key dired-mode-map (kbd "RET")
  (lambda ()
    (interactive)
    (if (file-directory-p (dired-get-file-for-visit))
        (diredp-find-file-reuse-dir-buffer)
      (dired-find-file-other-window))))
(define-key dired-mode-map (kbd "<C-kp-add>") 'custom/dired/create-dir-or-file)
(define-key dired-mode-map (kbd "<kp-add>") 'custom/dired/create-dir)
(define-key dired-mode-map (kbd "C-<up>") 'dired-prev-dirline)
(define-key dired-mode-map (kbd "C-<down>") 'dired-next-dirline)
(define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
(define-key dired-mode-map (kbd "I") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
(define-key dired-mode-map (kbd "C")
  (lambda ()
    (interactive)
    (call-interactively 'dired-do-copy)
    (dired-revert)))
(define-key dired-mode-map (kbd "D")
  (lambda ()
    (interactive)
    (call-interactively 'dired-do-delete)
    (dired-revert)))
(define-key dired-mode-map (kbd "R")
  (lambda ()
    (interactive)
    (call-interactively 'dired-do-rename)
    (dired-revert)))
;; call to (wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "C-SPC") 'dired-toggle-read-only)
(define-key wdired-mode-map (kbd "C-SPC") 'wdired-finish-edit)
(define-key wdired-mode-map (kbd "C-q") 'wdired-exit)
(define-key xref--button-map (kbd "C-<return>")
  'xref-show-location-at-point)

(add-hook 'dired-mode-hook 'auto-revert-mode)


(defun custom/dired/create-dir ()
  (interactive)
  (call-interactively 'dired-create-directory)
  (revert-buffer))

(defun custom/dired/create-dir-or-file ()
  (interactive)
  (let ((fname (ido-read-file-name "Create file or directory: "
                                   (dired-current-directory))))
    (if (s-ends-with? custom/fs-separator fname)
        (progn
          (dired-create-directory fname)
          (revert-buffer))
      (progn
        (f-touch fname)
        (revert-buffer)
        (dired-goto-file (expand-file-name fname))))))

(defun custom/dired/get-marked-nodes ()
  (if (equal major-mode 'dired-mode)
      (let ((files (dired-get-marked-files nil nil nil))
            (result nil))
        (dolist (file files)
          (push (list file
                      (f-file-p file)
                      (file-name-extension file))
                result))
        result)
    (error "Not in dired")))

(defun custom/find-free-file-name (file-name)
  (let ((counter 0)
        (run t)
        (new-name nil))
    (while run
      (setq new-name
            (concat file-name
                    ".copy" (format "%s" counter)))
      (if (not (file-exists-p new-name))
          (setq run nil)
        (setq counter (1+ counter))))
    new-name))

(defun dired-create-files (file-creator operation fn-list name-constructor
                                        &optional marker-char)
  "Create one or more new files from a list of existing files FN-LIST.
This function also handles querying the user, updating Dired
buffers, and displaying a success or failure message.

FILE-CREATOR should be a function.  It is called once for each
file in FN-LIST, and must create a new file, querying the user
and updating Dired buffers as necessary.  It should accept three
arguments: the old file name, the new name, and an argument
OK-IF-ALREADY-EXISTS with the same meaning as in `copy-file'.

OPERATION should be a capitalized string describing the operation
performed (e.g. `Copy').  It is used for error logging.

FN-LIST is the list of files to copy (full absolute file names).

NAME-CONSTRUCTOR should be a function accepting a single
argument, the name of an old file, and returning either the
corresponding new file name or nil to skip.

If optional argument MARKER-CHAR is non-nil, mark each
newly-created file's Dired entry with the character MARKER-CHAR,
or with the current marker character if MARKER-CHAR is t."
  (let (dired-create-files-failures failures
                                    skipped (success-count 0) (total (length fn-list)))
    (let (to overwrite-query
             overwrite-backup-query)	; for dired-handle-overwrite
      (dolist (from fn-list)
        (setq to (funcall name-constructor from))
        (if (equal to from)
            (setq to (custom/find-free-file-name to)))
        (if (not to)
            (setq skipped (cons (dired-make-relative from) skipped))
          (let* ((overwrite (file-exists-p to))
                 (dired-overwrite-confirmed ; for dired-handle-overwrite
                  (and overwrite
                       (let ((help-form '(format-message "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
                         (dired-query 'overwrite-query
                                      "Overwrite `%s'?" to))))
                 ;; must determine if FROM is marked before file-creator
                 ;; gets a chance to delete it (in case of a move).
                 (actual-marker-char
                  (cond  ((integerp marker-char) marker-char)
                         (marker-char (dired-file-marker from)) ; slow
                         (t nil))))
            ;; Handle the `dired-copy-file' file-creator specially
            ;; When copying a directory to another directory or
            ;; possibly to itself or one of its subdirectories.
            ;; e.g "~/foo/" => "~/test/"
            ;; or "~/foo/" =>"~/foo/"
            ;; or "~/foo/ => ~/foo/bar/")
            ;; In this case the 'name-constructor' have set the destination
            ;; TO to "~/test/foo" because the old emacs23 behavior
            ;; of `copy-directory' was to not create the subdirectory
            ;; and instead copy the contents.
            ;; With the new behavior of `copy-directory'
            ;; (similar to the `cp' shell command) we don't
            ;; need such a construction of the target directory,
            ;; so modify the destination TO to "~/test/" instead of "~/test/foo/".
            (let ((destname (file-name-directory to)))
              (when (and (file-directory-p from)
                         (file-directory-p to)
                         (eq file-creator 'dired-copy-file))
                (setq to destname))
              ;; If DESTNAME is a subdirectory of FROM, not a symlink,
              ;; and the method in use is copying, signal an error.
              (and (eq t (car (file-attributes destname)))
                   (eq file-creator 'dired-copy-file)
                   (file-in-directory-p destname from)
                   (error "Cannot copy `%s' into its subdirectory `%s'"
                          from to)))
            (condition-case err
                (progn
                  (funcall file-creator from to dired-overwrite-confirmed)
                  (if overwrite
                      ;; If we get here, file-creator hasn't been aborted
                      ;; and the old entry (if any) has to be deleted
                      ;; before adding the new entry.
                      (dired-remove-file to))
                  (setq success-count (1+ success-count))
                  (message "%s: %d of %d" operation success-count total)
                  (dired-add-file to actual-marker-char))
              (file-error		; FILE-CREATOR aborted
               (progn
                 (push (dired-make-relative from)
                       failures)
                 (dired-log "%s `%s' to `%s' failed:\n%s\n"
                            operation from to err))))))))
    (cond
     (dired-create-files-failures
      (setq failures (nconc failures dired-create-files-failures))
      (dired-log-summary
       (format "%s failed for %d file%s in %d requests"
               operation (length failures)
               (dired-plural-s (length failures))
               total)
       failures))
     (failures
      (dired-log-summary
       (format "%s failed for %d of %d file%s"
               operation (length failures)
               total (dired-plural-s total))
       failures))
     (skipped
      (dired-log-summary
       (format "%s: %d of %d file%s skipped"
               operation (length skipped) total
               (dired-plural-s total))
       skipped))
     (t
      (message "%s: %s file%s"
               operation success-count (dired-plural-s success-count)))))
  (revert-buffer))
