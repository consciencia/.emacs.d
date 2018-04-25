(advice-add #'file-exists-p
            :around (lambda (oldfn filename)
                      (if filename
                          (apply oldfn (list filename))
                        nil)))

(package-initialize)

;; set up elisp include dirs
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; set up EMACS custom settings file (used by UI settings manager)
(setq custom-file "~/.emacs.d/emacs-custom.el")

;; set up EMACS C code tracking
(setq find-function-C-source-directory
      (concat "~/.emacs.d/emacs-src/"
              (substring (emacs-version) 10 14)
              "/src"))

;; set up backup rules
(setq backup-directory-alist
      `(("." . "~/.emacs.d/file-backups")))
(setq delete-old-versions nil
      kept-new-versions 10
      kept-old-versions 10
      version-control t)

(if (eq system-type 'windows-nt)
    (setq custom/fs-separator "\\")
  (setq custom/fs-separator "/"))

;; load whole mode
(load "utils.el")
(load "package-loader.el")
(load "db.el")
(load "ui.el")
(load "keymap.el")
(load custom-file)

;; set up cygwin when running on windows
(if (eq system-type 'windows-nt)
    (progn
      (load "cygwin-mount.el")
      (require 'cygwin-mount)
      (cygwin-mount-activate)
      (load "setup-cygwin.el")
      (require 'setup-cygwin)))

;;----------------------------------------------
;; http://emacs-bootstrap.com/
;; update mode to be follow best practises
;;
;; C-x keybind is free for customization
;; C-p C-x is too free for customization
;;----------------------------------------------

;; add support for python
;; add support for javascript HTML CSS LESS
;; refactor whole thing
;; write some doc about it
