(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(if (file-directory-p "~/.emacs.d/extern/")
  (let ((default-directory  "~/.emacs.d/extern/"))
    (normal-top-level-add-subdirs-to-load-path)))
;; set up EMACS custom settings file (used by UI settings manager)
(setq custom-file "~/.emacs.d/emacs-custom.el")
;; set up EMACS C code tracking
(setq find-function-C-source-directory
      (concat "~/.emacs.d/emacs-src/"
              (substring (emacs-version) 10 14)
              "/src"))

(load "utils.el")

(load "package-init.el")
(custom/setup-packages)

(load "ui.el")
(custom/setup-ui)

(load "keymap.el")
(custom/setup-keymap)

(load custom-file)

(setq backup-directory-alist
      `(("." . "~/.emacs.d/file-backups")))
(setq delete-old-versions nil
      kept-new-versions 10
      kept-old-versions 10
      version-control t)

;; SETUP cygwin when running on windows
(if (eq system-type 'windows-nt)
    (progn
      (load "cygwin-mount.el")
      (require 'cygwin-mount)
      (cygwin-mount-activate)
      (load "setup-cygwin.el")
      (require 'setup-cygwin)))

;; These two are used for emacs state save before runs
;; not works correctly with neotree and minimap
;; some additional hacks must be done, maybe restarting minimap
;; and neotree after desktop load
;; in neotree, save somewhere current root and refresh it after its recreation
;; after desktop-read
;; to much work, ignore for now
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (desktop-save "~/.emacs.d/")
;; (desktop-read)

;; semantic maybe can load all DBs via semanticdb-current-database-list when
;; project roots are known (from projectile)
;; not sure, actual dir parsing is working same way, so maybe this is not needed

;; 9) setup python mode
;; 8) setup JS/HTML/CSS mode
;; install flycheck

