;; (package-initialize)
;; Placed here to make package manager happy and not insert
;; autogenerated shit here :(

;; (edebug-defun)
;; (setq debug-on-error t)
;; (toggle-debug-on-quit)

;; Detect chnages of emacs version and purge all caches which are
;; notoriously incompatible across emacs versions.
(let* ((write-file (lambda (filename data)
                     (ignore-errors
                       (let ((coding-system-for-write 'binary)
                             (write-region-annotate-functions nil)
                             (write-region-post-annotation-function nil))
                         (write-region data nil filename nil :silent)
                         nil))))
       (read-file (lambda (filename)
                    (ignore-errors
                      (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (setq buffer-file-coding-system 'binary)
                        (insert-file-contents-literally filename)
                        (buffer-substring-no-properties (point-min)
                                                        (point-max))))))
       (root (expand-file-name user-emacs-directory))
       (version-cache-filename (concat root "version.cache"))
       (version-cache (funcall read-file version-cache-filename)))
  (when (not (or (equal version-cache nil)
                 (equal version-cache (emacs-version))))
    (delete-directory (concat root "undo") t t)
    (delete-directory (concat root "semanticdb") t t)
    (delete-directory (concat root "elpa") t t))
  (funcall write-file version-cache-filename (emacs-version)))

(setq-default buffer-file-coding-system 'utf-8-unix)

;; set correct path separator
(if (eq system-type 'windows-nt)
    (setq custom/fs-separator "\\")
  (setq custom/fs-separator "/"))

;; set up Emacs custom settings file (used by UI settings manager)
(setq custom-file
      (concat (expand-file-name user-emacs-directory)
              "emacs-custom.el"))
(load custom-file)

;; set up Emacs C code tracking
(setq find-function-C-source-directory
      (concat (expand-file-name user-emacs-directory)
              "emacs-src"
              custom/fs-separator
              (substring (emacs-version) 10 14)
              custom/fs-separator
              "src"))

;; set up backup rules
(setq backup-directory-alist
      `(("." . ,(concat (expand-file-name user-emacs-directory)
                        "file-backups"))))

(setq delete-old-versions t
      kept-new-versions 100
      kept-old-versions 0
      version-control t)

;; Dont create lockfiles. They are pain in the ass in the
;; long run.
(setq create-lockfiles nil)

;; For speedup.
(setq inhibit-compacting-font-caches t)
(setq-default bidi-display-reordering nil)

(add-to-list 'load-path
             (concat (expand-file-name user-emacs-directory)
                     "lisp"))
(let ((default-directory
        (concat (expand-file-name user-emacs-directory)
                "lisp")))
  (normal-top-level-add-subdirs-to-load-path))

(setq package-check-signature 'allow-unsigned)
(package-initialize)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (error "SSL not supported!"))
  (add-to-list 'package-archives
               `("melpa" . ,(concat proto "://melpa.org/packages/"))))

(unless (version< emacs-version "28")
  ;; Native compilation broken markdown mode.
  (setq native-comp-deferred-compilation-deny-list
        '("markdown-mode\\.el$")))

(load "packages.el")
(load "utils.el")
(load "db.el")
(load "ui.el")
(load "keymap.el")
(load "edebug-tweaks.el")
(load "shell-tweaks.el")
(load (concat (expand-file-name user-emacs-directory)
              "local-init.el")
      ;; Causes load to not fail when target file does
      ;; not exists.
      t)

;; When enabled, its faster but it eats a lot more memory.
;; Its up to you.
(setq *custom/enable-lazy-gc* nil)
(if *custom/enable-lazy-gc*
    (progn
      ;; old value was 800000 (< 1MB)
      ;; new one is 512MB with GC cycle every
      ;; 5 second of iddling
      (setq gc-cons-threshold 536870912)
      (run-with-idle-timer 5 t
                           (lambda ()
                             (garbage-collect)))))

(setq create-lockfiles nil)

(global-auto-revert-mode)

(if (version< emacs-version "28")
    ;; Byte compile all functions defined in emacs config.
    (mapatoms (lambda (sym)
                (when (and (s-starts-with-p "custom/" (format "%s" sym))
                           (fboundp sym))
                  (let ((byte-compile-log-warning-function
                         (lambda (&rest args))))
                    (byte-compile sym)))))
  ;; Native compile all functions defined in emacs config.
  (mapatoms (lambda (sym)
              (when (and (s-starts-with-p "custom/" (format "%s" sym))
                         (fboundp sym))
                (let ((byte-compile-log-warning-function
                       (lambda (&rest args))))
                  (ignore-error (native-compile sym)))))))
