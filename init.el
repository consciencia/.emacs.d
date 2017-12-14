;; set up ELISP load paths, extern subtree s used for inclusion of
;; private config tree (for example, some sensitive work related infos)
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

;; install GNU Global (add one for ubuntu, redhat and arch)
;; fedora: sudo dnf install global
;; in ext-apps is windows binary of global

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

;;;;;;;;;
;; projectile-project-root
;; get project root from projectile will be handy when managing
;; TAGS indexes and things like that

;; 1) find how to bind C-j in scratch buffer
;; 2) fix rebinding of C-j to C-RET for IDO accroding to answer in forum
;; 3) find out why arrow in dired not works in terminal mode
;; 4) find out more info about C horrible coloring
;; 5) Install support for multiple cursors
;; 6) set up magit
;; 7) set up full C/C++ dev environment
;; 8) Do same for JS
;; 9) And for python
