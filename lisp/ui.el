(require 'whitespace)

(which-function-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'spacemacs-dark t)

(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 2)

(global-linum-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset tab-width)
(c-set-offset 'substatement-open 0)
(c-set-offset 'case-label tab-width nil)
(setq-default tab-stop-list (number-sequence tab-width 120 tab-width))
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(setq inhibit-startup-screen t)
(setq inhibit-default-init t)
(setq-default frame-title-format "%b (%f)")
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode t)

(add-hook 'buffer-list-update-hook
          'custom/on-buffer-list-change)

(add-hook 'prog-mode-hook
          'whitespace-mode)
(set-face-attribute 'default nil :height 100)
(setq scroll-conservatively 101)

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(fset 'yes-or-no-p 'y-or-n-p)
(advice-add 'y-or-n-p :around #'y-or-n-p-with-return)

(setq
 gdb-many-windows t
 gdb-show-main t)

(setq split-height-threshold 40)
(setq split-width-threshold 80)

(blink-cursor-mode 0)
(setq visible-cursor nil)

(global-visual-line-mode t)
