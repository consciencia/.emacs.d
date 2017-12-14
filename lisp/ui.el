(defun custom/setup-ui ()
  (which-function-mode t)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (load-theme 'wheatgrass)
  (setq ring-bell-function 'ignore)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
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
  (set-face-attribute 'default nil :height 100)
  (setq scroll-conservatively 101)
  
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (setq gc-cons-threshold 800000)))

  (setq tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t)
  
  (fset 'yes-or-no-p 'y-or-n-p)
  (advice-add 'y-or-n-p :around #'y-or-n-p-with-return)

  (setq
   gdb-many-windows t
   gdb-show-main t)

  (setq split-height-threshold 40)
  (setq split-width-threshold 1000))
