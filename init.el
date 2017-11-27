(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq custom-file "~/.emacs.d/emacs-custom.el")

(require 'cl)
(require 'package)
(require 'whitespace)

; Add MELPA repository
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

; Install packages when missing
(setq used-packages
      '(undo-tree
        ace-window
        projectile
        dired+
        neotree
        f
        autopair
        minimap))
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (progn (message "installing %s" package)
            (package-refresh-contents)
            (package-install package))))
 used-packages)

;; (byte-recompile-directory "~/.emacs.d/elpa/" nil 'force)

(require 'dired+)
(require 'neotree)
(require 'autopair)
(require 'ido) 
(require 'minimap) 

; UI tweaks
(which-function-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'wheatgrass)
(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(global-linum-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(c-set-offset 'substatement-open 0)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq inhibit-startup-screen t)
(setq inhibit-default-init t)
(setq-default frame-title-format "%b (%f)")
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; undo tree
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)

; dired
(add-hook 'dired-load-hook
          (lambda ()
          	(load "dired-x")))
(diredp-toggle-find-file-reuse-dir 1)

; projectile
(projectile-mode)

; neotree
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(defun neotree-startup ()
  (interactive)
  (neotree-show)
  (call-interactively 'other-window))
(if (daemonp)
    (add-hook 'server-switch-hook #'neotree-startup)
  (add-hook 'after-init-hook #'neotree-startup))

; autopair
(autopair-global-mode) 

; ido
(ido-mode t)

; minimap
(minimap-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; load other init scripts
(load "utils.el")
(load "custom-symbol-navigation.el")
(load "keymap.el")

; load customization
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;; describe-key
;; describes used key (especially handy when trying to find
;; code for some exotic key)
;; call-interactively
