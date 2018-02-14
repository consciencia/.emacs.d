(load "symbol-navigation.el")
(require 'elisp-slime-nav)

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
(global-set-key (kbd "C-+") 'custom/scroll-up)
(global-set-key (kbd "C--") 'custom/scroll-down)
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
(define-key minibuffer-local-completion-map
  (kbd "<tab>")
  (lambda ()
    (interactive)
    (call-interactively 'minibuffer-complete)
    (call-interactively 'switch-to-completions)
    (call-interactively 'isearch-forward)))
(define-key completion-list-mode-map
  (kbd "C-f")
  'isearch-forward)

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
(global-set-key (kbd "C-5")
                (lambda ()
                  (interactive)
                  (call-interactively 'mark-defun)
                  (setq transient-mark-mode (cons 'only transient-mark-mode))))
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
(define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)

;; PROJECT MANAGEMENT BINDS
(define-key projectile-key-map (kbd "C-d C-s")
  (lambda ()
    (interactive)
    (projectile-switch-project)))
(define-key projectile-key-map (kbd "C-d C-a")
  (lambda ()
    (interactive)
    (let ((proj-root (call-interactively 'custom/projectile-add-known-project))
          (proj-type (custom/get-simple-input "Project type: "
                                              '("C/C++ (generic)"
                                                "Other"))))
      (if (and proj-root proj-type) 
          (custom/project/generate-loader proj-root
                                          proj-type)))))

(define-key projectile-key-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let* ((projectile-completion-system #'custom/default-completing-read))
      (call-interactively 'projectile-find-file))))
(define-key projectile-key-map (kbd "C-f C-f") 'projectile-grep)
(define-key projectile-key-map (kbd "C-f C-r") 'projectile-replace)
(define-key projectile-key-map (kbd "C-f C-o")
  (lambda ()
    (interactive)
    (call-interactively 'projectile-multi-occur)
    (switch-to-buffer-other-window "*Occur*")
    (shrink-window-if-larger-than-buffer)))
(define-key projectile-key-map (kbd "C-v C-s") 'magit-status)
(define-key projectile-key-map (kbd "C-v C-l") 'magit-log-popup)
(define-key projectile-key-map (kbd "C-v C-c") 'magit-branch-and-checkout)
(define-key projectile-key-map (kbd "C-v C-d") 'magit-diff-popup)
(define-key projectile-key-map (kbd "C-v C-b C-s") 'magit-blame)
(define-key projectile-key-map (kbd "C-v C-b C-p") 'magit-blame-popup)
(define-key projectile-key-map (kbd "C-v C-b C-q") 'magit-blame-quit)
(global-set-key (kbd "C-S--") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-S-+") 'git-gutter:next-hunk)

;; BUFFER LOCAL SEARCHING BINDS
(define-key search-key-map (kbd "C-r") 'vr/query-replace)
(define-key search-key-map (kbd "C-f") 'isearch-forward)
(define-key search-key-map (kbd "C-s") 'isearch-forward-symbol-at-point)
(define-key search-key-map (kbd "C-o")
  (lambda ()
    (interactive)
    (call-interactively 'occur)
    (switch-to-buffer-other-window "*Occur*")
    (shrink-window-if-larger-than-buffer)))
(define-key search-key-map (kbd "C-g") 'grep)
(add-hook 'isearch-mode-hook 'custom/enhance-isearch)
(define-key query-replace-map (kbd "<return>") 'act)

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

;; C BINDS
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-d") 'custom/mark-whole-word)
            (local-set-key (kbd "M-.") 'custom/semantic-goto-definition)
            (local-set-key (kbd "M-,") 'custom/semantic-pop-tag-mark)
            (local-set-key (kbd "M--")
                           (lambda ()
                             (interactive)
                             (semantic-symref-symbol (thing-at-point 'symbol t))))
            (local-set-key (kbd "C-*")
                           (lambda ()
                             (interactive)
                             (call-interactively 'semantic-ia-show-doc)
                             (switch-to-buffer-other-window "*TAG DOCUMENTATION*")))
            (local-set-key (kbd "C-,") 'custom/semantic-switch-proto)
            (local-set-key (kbd "C-.") 'semantic-mrub-switch-tags)))

;; C++ BINDS
(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-d") 'custom/mark-whole-word)
            (local-set-key (kbd "M-.") 'custom/semantic-goto-definition)
            (local-set-key (kbd "M-,") 'custom/semantic-pop-tag-mark)
            (local-set-key (kbd "M--")
                           (lambda ()
                             (interactive)
                             (semantic-symref-symbol (thing-at-point 'symbol t))))
            (local-set-key (kbd "C-*")
                           (lambda ()
                             (interactive)
                             (call-interactively 'semantic-ia-show-doc)
                             (switch-to-buffer-other-window "*TAG DOCUMENTATION*")))
            (local-set-key (kbd "C-,") 'custom/semantic-switch-proto)
            (local-set-key (kbd "C-.") 'semantic-mrub-switch-tags)))

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
    (define-key map (kbd "<C-return>") 'custom/special-c-return-handler)
    (define-key map (kbd "<M-return>") 'custom/special-m-return-handler)
    map))
(with-eval-after-load 'ido
  (define-key ido-common-completion-map
    (kbd "<C-return>")
    'custom/special-c-return-handler)
  (define-key ido-common-completion-map
    (kbd "<M-return>")
    'custom/special-m-return-handler))
(add-to-ordered-list 'emulation-mode-map-alists
                     `((cua-mode . ,custom-ido-map))
                     0)

(require 'company)
(define-key company-active-map (kbd "<tab>") 'company-search-candidates)


