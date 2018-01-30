(load "symbol-navigation.el")
(require 'elisp-slime-nav)

(defun custom/setup-keymap ()
  (setq local-function-key-map
        (delq '(kp-tab . [9]) local-function-key-map))

  ;; CUA SETUP
  (cua-mode t)
  (setq cua-auto-tabify-rectangles nil)
  (transient-mark-mode 1)
  (setq cua-keep-region-after-copy t)

  ;; KEYMAPS SETUP
  (define-prefix-command 'general-key-map)
  (global-set-key (kbd "C-e") 'general-key-map)
  (define-prefix-command 'regs-key-map)
  (global-set-key (kbd "C-r") 'regs-key-map)
  (define-prefix-command 'bookmarks-key-map)
  (global-set-key (kbd "C-b") 'bookmarks-key-map)
  (setq bookmark-save-flag 1)
  (define-prefix-command 'projectile-key-map)
  (global-set-key (kbd "C-p") 'projectile-key-map)
  (define-prefix-command 'search-key-map)
  (global-set-key (kbd "C-f") 'search-key-map)
  (define-prefix-command 'ace-jump-key-map)
  (global-set-key (kbd "C-g") 'ace-jump-key-map)

  ;; GENERAL BINDS
  (global-set-key (kbd "<f2>") 'neotree-toggle)
  (global-set-key (kbd "<f12>") 'customize-group)
  (global-set-key (kbd "<f8>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-+")
                  (lambda ()
                    (interactive)
                    (scroll-up 4)))
  (global-set-key (kbd "C--")
                  (lambda ()
                    (interactive)
                    (scroll-down 4)))
  (global-set-key (kbd "C-S-+")
                  (lambda ()
                    (interactive)
                    (scroll-other-window 4)))
  (global-set-key (kbd "C-S--")
                  (lambda ()
                    (interactive)
                    (scroll-other-window-down 4)))
  (define-key general-key-map (kbd "C-l C-b") 'list-buffers)
  (define-key general-key-map (kbd "C-l C-p") 'list-packages)
  (define-key regs-key-map (kbd "C-c") 'copy-to-register)
  (define-key regs-key-map (kbd "C-v") 'insert-register)
  (define-key regs-key-map (kbd "C-a") 'append-to-register)
  (define-key regs-key-map (kbd "C-p") 'prepend-to-register)
  (define-key regs-key-map (kbd "C-l") 'list-registers)
  (define-key general-key-map (kbd "C-r C-h") 'hexl-mode)
  (define-key general-key-map (kbd "C-r C-c") 'compile)
  (define-key general-key-map (kbd "C-r C-d") 'dired)
  (define-key general-key-map (kbd "C-r C-g") 'gdb)
  (define-key general-key-map (kbd "C-r C-m") 'man)
  (define-key global-map
    (kbd "<escape> <escape> <escape>")
    'custom/keyboard-escape-quit)
  
  ;; WINDOWS AND FRAMES MANIPULATION BINDS
  (define-key general-key-map (kbd "C-s C-h") 'split-window-horizontally)
  (define-key general-key-map (kbd "C-s C-v") 'split-window-vertically)
  (global-set-key (kbd "M-<left>") (ignore-error-wrapper 'windmove-left))
  (global-set-key (kbd "M-<right>") (ignore-error-wrapper 'windmove-right))
  (global-set-key (kbd "M-<up>") (ignore-error-wrapper 'windmove-up))
  (global-set-key (kbd "M-<down>") (ignore-error-wrapper 'windmove-down))
  ;; not needed now, arrows are good enough
  ;; (global-set-key (kbd "C-5") 'ace-window)
  (define-key general-key-map (kbd "C-f C-o") 'other-frame)
  (define-key general-key-map (kbd "C-f C-n") 'make-frame)
  (global-set-key (kbd "C-q") 'custom/universal-quit) 

  ;; BUFFER MANIPULATION BINDS
  (global-set-key (kbd "C-o") 'find-file)
  (global-set-key (kbd "C-<prior>") 'next-buffer)
  (global-set-key (kbd "C-<next>") 'previous-buffer)
  (global-set-key (kbd "C-0") 'ido-switch-buffer)
  (global-set-key (kbd "C-y") nil)
  (global-set-key (kbd "C-s") 'save-buffer)
  (global-set-key (kbd "C-w") 'custom/kill-buffer)

  ;; GENERIC TEXT MANIPULATION BINDS
  (global-set-key (kbd "C-a") 'custom/mark-whole-buffer)
  (global-set-key (kbd "C-d") 'custom/mark-whole-word)
  (global-set-key (kbd "C-l") 'custom/mark-whole-line)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'redo)
  (global-set-key (kbd "C-u") 'undo-tree-visualize)
  (global-set-key (kbd "C-<right>") 'custom/forward-symbol)
  (global-set-key (kbd "C-<left>") 'custom/backward-symbol)
  (global-set-key (kbd "C-<down>") 'forward-paragraph)
  (global-set-key (kbd "C-<up>") 'backward-paragraph)
  (global-set-key (kbd "C-5") 'recenter)
  (define-key ace-jump-key-map (kbd "C-g") 'goto-line)
  (global-set-key (kbd "C-<kp-divide>") 'comment-or-uncomment-region)
  (global-set-key (kbd "M-c") 'comment-or-uncomment-region)
  (define-key bookmarks-key-map (kbd "C-j") 'bookmark-jump)
  (define-key bookmarks-key-map (kbd "C-s") 'bookmark-set)
  (define-key bookmarks-key-map (kbd "C-l") 'list-bookmarks)
  (define-key bookmarks-key-map  (kbd "C-b") 'ido-switch-buffer)
  (define-key ace-jump-key-map  (kbd "C-l") 'ace-jump-line-mode)
  (define-key ace-jump-key-map (kbd "C-w") 'ace-jump-word-mode)
  (define-key ace-jump-key-map (kbd "C-c") 'ace-jump-char-mode)
  (define-key ace-jump-key-map (kbd "C-z") 'ace-jump-mode-pop-mark)
  (global-set-key (kbd "C-SPC") 'ace-jump-word-mode)
  (global-set-key (kbd "C-M-SPC") 'ace-jump-mode-pop-mark)
  (define-key regs-key-map (kbd "C-r") 'ido-goto-symbol)
  (global-set-key (kbd "M-m") 'custom/mc/mark-next-like-this)
  (global-set-key (kbd "M-n") 'custom/mc/mark-prev-like-this)
  (define-key mc/keymap (kbd "<escape> <escape> <escape>") 'mc/keyboard-quit)
  
  ;; PROJECT MANAGEMENT BINDS
  (define-key projectile-key-map (kbd "C-d C-s") 'projectile-switch-project)
  (define-key projectile-key-map (kbd "C-d C-a") 'projectile-add-known-project)
  (define-key projectile-key-map (kbd "C-d C-i C-u")
    (lambda ()
      (interactive)
      (cond
       ((equal major-mode 'c-mode)
        (call-interactively 'ggtags-update-tags))
       ((equal major-mode 'c++-mode)
        (call-interactively 'ggtags-update-tags))
       (t (message "Current mode doesnt support index update")))))
  (define-key projectile-key-map (kbd "C-d C-i C-c")
    (lambda ()
      (interactive)
      (cond
       ((equal major-mode 'c-mode)
        (call-interactively 'ggtags-create-tags))
       ((equal major-mode 'c++-mode)
        (call-interactively 'ggtags-create-tags))
       (t (message "Current mode doesnt support index creation"))))) 
  (define-key projectile-key-map (kbd "C-o")
    (lambda ()
      (interactive)
      (let* ((projectile-completion-system #'completing-read-default))
        (call-interactively 'projectile-find-file))))
  (define-key projectile-key-map (kbd "C-f C-f") 'projectile-grep)
  (define-key projectile-key-map (kbd "C-f C-r") 'projectile-replace)
  (define-key projectile-key-map (kbd "C-f C-o") 'projectile-multi-occur)
  
  ;; BUFFER LOCAL SEARCHING BINDS
  (define-key search-key-map (kbd "C-r") 'vr/query-replace)
  (define-key search-key-map (kbd "C-f") 'isearch-forward)
  (define-key search-key-map (kbd "C-s") 'isearch-forward-symbol-at-point)
  (define-key search-key-map (kbd "C-o") 'occur)
  (define-key search-key-map (kbd "C-g") 'grep)
  (add-hook 'isearch-mode-hook (lambda ()
                                 (define-key isearch-mode-map
                                   (kbd "C-<right>")
                                   'isearch-repeat-forward)
                                 (define-key isearch-mode-map
                                   (kbd "C-<left>")
                                   'isearch-repeat-backward)))
  (define-key query-replace-map (kbd "<return>") 'act)
  
  ;; GGTAGS BINDS
  (add-hook 'ggtags-global-mode-hook
            (lambda ()
              (local-set-key (kbd "C-<down>") 'compilation-next-error)
              (local-set-key (kbd "C-<up>") 'compilation-previous-error)
              (local-set-key (kbd "C-<right>") 'compilation-next-file)
              (local-set-key (kbd "C-<left>") 'compilation-previous-file)))
  
  ;; ELISP BINDS
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l C-d C-k") 'describe-key)
              (local-set-key (kbd "C-l C-d C-f") 'describe-function)
              (local-set-key (kbd "C-l C-d C-v" ) 'describe-variable)
              (local-set-key (kbd "C-l C-d C-p" ) 'describe-package)
              (local-set-key (kbd "C-l C-e C-s") 'eval-last-sexp)
              (local-set-key (kbd "C-l C-e C-f") 'eval-defun)
              (local-set-key (kbd "C-l C-e C-r") 'eval-region)
              (local-set-key (kbd "C-l C-e C-b") 'eval-buffer)
              (local-set-key (kbd "C-l C-e C-e") 'eval-expression)
              (local-set-key (kbd "C-l C-e C-p") 'eval-print-last-sexp)
              (local-set-key (kbd "C-l C-f C-l") 'find-library)
              (local-set-key (kbd "C-l C-i") 'ielm)
              (local-set-key (kbd "C-l C-l") 'custom/mark-whole-line)
              (local-set-key (kbd "M--") 'find-function-on-key)
              (custom-paredit-init)))
  (add-hook 'eval-expression-minibuffer-setup-hook
            (lambda ()
              (custom-paredit-init)))
  ;; define M-. and M-, for jumping around elips sources (funs and vars)
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))

  ;; COMMON LISP BINDS
  (add-hook 'lisp-mode-hook
            (lambda ()
              (custom-paredit-init)))
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (custom-paredit-init)))

  ;; SCHEME BINDS
  (add-hook 'scheme-mode-hook
            (lambda ()
              (custom-paredit-init)))
  
  ;; C GENERAL BINDS
  (setq custom/c-navigation-method-used "global")
  (define-key ggtags-mode-map (kbd "C-,")
    (lambda ()
      (interactive)
      (if (equal custom/c-navigation-method-used "global")
          (progn
            (message "NAVIGATION MODE SEMANTIC SET")
            (setq custom/c-navigation-method-used "semantic"))
        (progn
          (message "NAVIGATION MODE GLOBAL SET")
          (setq custom/c-navigation-method-used "global")))))
  (define-key ggtags-mode-map (kbd "M-.")
    (lambda ()
      (interactive)
      (if (equal custom/c-navigation-method-used "global")
          (progn
            (call-interactively 'ggtags-find-definition))
        (progn
          (call-interactively 'custom/semantic-goto-definition)))))
  (define-key ggtags-mode-map (kbd "C-.")
    (lambda ()
      (interactive)
      (if (equal custom/c-navigation-method-used "global")
          (progn
            (call-interactively 'ggtags-find-reference))
        (progn
          (semantic-symref-symbol (thing-at-point 'symbol t))))))
  (define-key ggtags-mode-map (kbd "M-,")
    (lambda ()
      (interactive)
      (if (equal custom/c-navigation-method-used "semantic")
          (call-interactively 'custom/semantic-pop-tag-mark)
        (if (equal custom/c-navigation-method-used "global")
            (call-interactively 'pop-tag-mark)
          (error "Unknown navigation method %s" custom/c-navigation-method-used)))))
  (define-key ggtags-mode-map (kbd "<M-kp-multiply>") 'semantic-ia-show-doc)   

  ;; semantic-analyze-proto-impl-toggle
  ;; -> Toggle between the implementation, and a prototype of tag under point 
  
  ;; C BINDS
  (add-hook 'c-mode-hook
            (lambda ()

              ))
  (add-hook 'c-mode-hook 'semantic-mode)
  
  ;; C++ BINDS
  (add-hook 'c++-mode-hook
            (lambda ()

              ))
  (add-hook 'c++-mode-hook 'semantic-mode)
  
  ;; HEXL BINDS
  (add-hook 'hexl-mode-hook
            (lambda ()
              (local-set-key (kbd "C-g") 'hexl-goto-hex-address)
              (local-set-key (kbd "C-w") 'custom/kill-buffer)))
  
  ;; NEOTREE BINDS
  (define-key neotree-mode-map (kbd "C-f C-n") 'neotree-create-node)
  (define-key neotree-mode-map (kbd "C-f C-d") 'neotree-delete-node)
  (define-key neotree-mode-map (kbd "C-f C-r") 'neotree-rename-node)
  (define-key neotree-mode-map (kbd "C-f C-c") 'neotree-copy-node)
  (define-key neotree-mode-map (kbd "C-f C-h") 'neotree-hidden-file-toggle)
  (define-key neotree-mode-map (kbd "C-<up>") 'neotree-select-up-node)
  (define-key neotree-mode-map (kbd "C-<down>") 'neotree-select-down-node)
  (define-key neotree-mode-map (kbd "C-<right>") 'neotree-select-next-sibling-node)
  (define-key neotree-mode-map (kbd "C-<left>") 'neotree-select-previous-sibling-node)

  ;; IDO BINDS
  (global-set-key (kbd "M-x")
                  (lambda ()
                    (interactive)
                    (call-interactively
                     (intern
                      (ido-completing-read
                       "M-x "
                       (all-completions "" obarray 'commandp))))))
  (defvar custom-ido-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<C-return>") 'ido-select-text)
      (define-key map (kbd "<M-return>") 'ido-magic-forward-char)
      map))
  (with-eval-after-load 'ido
    (define-key ido-common-completion-map (kbd "<C-return>") 'ido-select-text)
    (define-key ido-common-completion-map (kbd "<M-return>") 'ido-magic-forward-char))
  (add-to-ordered-list 'emulation-mode-map-alists
                       `((cua-mode . ,custom-ido-map))
                       0)

  (require 'company)
  (define-key company-active-map (kbd "<tab>") 'company-search-candidates))


