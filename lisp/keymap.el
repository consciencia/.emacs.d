(load "symbol-navigation.el")
(require 'elisp-slime-nav)

(setq local-function-key-map
      (delq '(kp-tab . [9])
            local-function-key-map))

;; CUA SETUP
(cua-mode t)
(transient-mark-mode 1)
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)
(setq bookmark-save-flag 1)

;; KEYMAPS SETUP
(define-prefix-command 'general-key-map)
(global-set-key (kbd "C-e") 'general-key-map)
(define-prefix-command 'regs-key-map)
(define-key general-key-map (kbd "C-r") 'regs-key-map)
(define-prefix-command 'bookmarks-key-map)
(define-key general-key-map (kbd "C-b") 'bookmarks-key-map)
(define-prefix-command 'projectile-key-map)
(global-set-key (kbd "C-p") 'projectile-key-map)
(define-prefix-command 'search-key-map)
(global-set-key (kbd "C-f") 'search-key-map)

;; GENERAL BINDS
(global-set-key (kbd "<insert>") nil)
(global-set-key (kbd "<f2>") 'neotree-toggle)
(global-set-key (kbd "<f3>") 'neotree-find)
(global-set-key (kbd "<f4>")
                (lambda ()
                  (interactive)
                  (if (equal major-mode 'dired-mode)
                      (call-interactively 'dired-other-window)
                    (call-interactively 'dired-jump-other-window))))
(global-set-key (kbd "<f5>") 'custom/shell-at-dir)
(global-set-key (kbd "<f12>") 'customize-group)
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "<f10>") 'customize-face)
(global-set-key (kbd "<next>") 'custom/scroll-up)
(global-set-key (kbd "<prior>") 'custom/scroll-down)
(define-key general-key-map (kbd "C-l C-b") 'list-buffers)
(define-key general-key-map (kbd "C-l C-p") 'list-packages)
(define-key general-key-map (kbd "C-l C-r") 'list-registers)
(define-key general-key-map (kbd "C-l C-a") 'list-abbrevs)
(define-key general-key-map (kbd "C-l C-k") 'browse-kill-ring)
(define-key general-key-map (kbd "C-l C-s")
  (lambda ()
    (interactive)
    ;; Native alternative is `describe-bindings'.
    (custom/with-simple-pop-up "*TOP LEVEL KEYS*"
      (insert (car (which-key--process-page
                    (which-key--list-to-pages
                     (which-key--get-bindings) 150 240)))))))
(define-key general-key-map (kbd "C-d C-k") 'describe-key)
(define-key general-key-map (kbd "C-d C-d") 'describe-function)
(define-key general-key-map (kbd "C-d C-p" ) 'describe-package)
(define-key general-key-map (kbd "C-d C-f" ) 'apropos)
(define-key general-key-map (kbd "C-p C-s") 'profiler-start)
(define-key general-key-map (kbd "C-p C-p")
  (lambda ()
    (interactive)
    (call-interactively 'profiler-report)
    (call-interactively 'profiler-stop)))
(define-key general-key-map (kbd "C-f C-f") 'find-function)
(define-key general-key-map (kbd "C-f C-k") 'find-function-on-key)
(define-key general-key-map (kbd "C-f C-l") 'find-library)
(define-key general-key-map (kbd "C-i") 'ielm)
(define-key general-key-map (kbd "C-u") 'custom/toggle-uis)
(define-key browse-kill-ring-mode-map (kbd "C-<down>") 'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map (kbd "C-<up>") 'browse-kill-ring-previous)
(define-key browse-kill-ring-mode-map (kbd "<down>") 'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map (kbd "<up>") 'browse-kill-ring-previous)
(define-key browse-kill-ring-mode-map (kbd "C-<left>") 'custom/nop)
(define-key browse-kill-ring-mode-map (kbd "C-<right>") 'custom/nop)
(define-key browse-kill-ring-mode-map (kbd "<left>") 'custom/nop)
(define-key browse-kill-ring-mode-map (kbd "<right>") 'custom/nop)
(define-key browse-kill-ring-mode-map (kbd "C-o") 'browse-kill-ring-occur)
(define-key regs-key-map (kbd "C-c") 'copy-to-register)
(define-key regs-key-map (kbd "C-v") 'insert-register)
(define-key regs-key-map (kbd "C-a") 'append-to-register)
(define-key regs-key-map (kbd "C-p") 'prepend-to-register)
(define-key regs-key-map (kbd "C-l") 'list-registers)
(define-key completion-list-mode-map
  (kbd "C-f")
  'isearch-forward)

;; WINDOWS AND FRAMES MANIPULATION BINDS
(define-key general-key-map (kbd "C-w C-<right>")
  (lambda ()
    (interactive)
    (split-window-horizontally)
    (windmove-right)))
(define-key general-key-map (kbd "C-w C-<left>")
  (lambda ()
    (interactive)
    (split-window-horizontally)))
(define-key general-key-map (kbd "C-w C-<down>")
  (lambda ()
    (interactive)
    (split-window-vertically)
    (windmove-down)))
(define-key general-key-map (kbd "C-w C-<up>")
  (lambda ()
    (interactive)
    (split-window-vertically)))
(define-key general-key-map (kbd "C-w C-s")
  (lambda ()
    (interactive)
    (let* ((this (selected-window))
           (other (next-window))
           (this-buffer (window-buffer this))
           (other-buffer (window-buffer other)))
      (set-window-buffer other this-buffer)
      (set-window-buffer this other-buffer))))
(global-set-key (kbd "M-<left>") (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "M-<right>") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "M-<up>") (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "M-<down>") (ignore-error-wrapper 'windmove-down))
(global-set-key (kbd "M-5") 'shrink-window-if-larger-than-buffer)
(global-set-key (kbd "C-5") 'fit-window-to-buffer)
(global-set-key (kbd "M-8") 'enlarge-window)
(global-set-key (kbd "M-2") 'shrink-window)
(global-set-key (kbd "M-6") 'enlarge-window-horizontally)
(global-set-key (kbd "M-4") 'shrink-window-horizontally)
(define-key general-key-map (kbd "C-f C-o") 'other-frame)
(define-key general-key-map (kbd "C-f C-n") 'make-frame)
(global-set-key (kbd "C-q") 'custom/universal-quit)

;; BUFFER MANIPULATION BINDS
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-S-<prior>") 'next-buffer)
(global-set-key (kbd "C-S-<next>") 'previous-buffer)
(global-set-key (kbd "C-0") 'ido-switch-buffer)
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-y") nil)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'save-some-buffers)
(global-set-key (kbd "C-w") 'custom/kill-buffer)
(define-key magit-status-mode-map (kbd "C-w") 'magit-mode-bury-buffer)

;; GENERIC TEXT MANIPULATION BINDS
(global-set-key (kbd "C-a") 'custom/mark-whole-buffer)
(global-set-key (kbd "C-d") 'custom/mark-whole-word)
(global-set-key (kbd "C-l") 'custom/mark-whole-line)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "C-u")
                (lambda ()
                  (interactive)
                  (call-interactively 'undo-tree-visualize)
                  (call-interactively 'undo-tree-visualizer-toggle-diff)))
(global-set-key (kbd "C-<right>") 'custom/forward-symbol)
(global-set-key (kbd "C-<left>") 'custom/backward-symbol)
(global-set-key (kbd "C-<down>") 'forward-paragraph)
(global-set-key (kbd "C-<up>") 'backward-paragraph)
(global-set-key (kbd "C-SPC") 'custom/avy-jump-char-mode)
(define-key c++-mode-map (kbd "C-SPC") 'custom/avy-jump-char-mode)
(define-key c-mode-map (kbd "C-SPC") 'custom/avy-jump-char-mode)
(define-key python-mode-map (kbd "C-SPC") 'custom/avy-jump-char-mode)

(global-set-key (kbd "C-g") 'custom/goto-line)
(global-set-key (kbd "C-<kp-divide>") 'comment-or-uncomment-region)
(global-set-key (kbd "M-c") nil)
(define-key bookmarks-key-map (kbd "C-j")
  (lambda ()
    (interactive)
    (call-interactively 'bookmark-jump)
    (recenter)))
(define-key bookmarks-key-map (kbd "C-s") 'bookmark-set)
(define-key bookmarks-key-map (kbd "C-l") 'list-bookmarks)
(global-set-key (kbd "C-r") 'ido-goto-symbol)
(global-set-key (kbd "C-<tab>") 'custom/mc/mark-next-like-this)
(global-set-key (kbd "C-S-<iso-lefttab>") 'custom/mc/mark-prev-like-this)
(define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)
(global-set-key (kbd "M-r") 'cua-rectangle-mark-mode)

;; PROJECT MANAGEMENT BINDS
(define-key projectile-key-map (kbd "C-d C-s") 'projectile-switch-project)
(define-key projectile-key-map (kbd "C-d C-a")
  (lambda ()
    (interactive)
    (let ((proj-root (call-interactively 'custom/projectile-add-known-project))
          (proj-type (custom/get-simple-input "Project type: "
                                              '("Javascript"
                                                "Other"))))
      (if (and proj-root proj-type)
          (custom/project/generate-loader proj-root
                                          proj-type)))))
(define-key projectile-key-map (kbd "C-d C-k") 'projectile-kill-buffers)
(define-key projectile-key-map (kbd "C-d C-r") 'projectile-remove-known-project)
(define-key projectile-key-map (kbd "C-d C-o") 'projectile-find-other-file)
(define-key projectile-key-map (kbd "C-d C-d")
  (lambda ()
    (interactive)
    (dired-other-window (projectile-project-root))))
(define-key projectile-key-map (kbd "C-d C-c")
  (lambda ()
    (interactive)
    (cond ((or (equal major-mode 'c-mode)
               (equal major-mode 'c++-mode))
           (ede-compile-project))
          (t (error "Compilation is not supported in %s"
                    major-mode)))))
(define-key projectile-key-map (kbd "C-o") 'projectile-find-file)
(global-set-key (kbd "C-t") 'projectile-find-file)
(define-key projectile-key-map (kbd "C-f C-f") 'projectile-grep)
(define-key projectile-key-map (kbd "C-f C-r") 'projectile-replace)
(define-key projectile-key-map (kbd "C-f C-o")
  (lambda ()
    (interactive)
    (call-interactively 'projectile-multi-occur)
    (switch-to-buffer-other-window "*Occur*")
    (shrink-window-if-larger-than-buffer)))
(define-key projectile-key-map (kbd "C-v C-s") 'magit-status)
(define-key projectile-key-map (kbd "C-v C-l C-l") 'magit-log-popup)
(define-key projectile-key-map (kbd "C-v C-l C-f") 'magit-log-buffer-file)
(define-key projectile-key-map (kbd "C-v C-c") 'magit-branch-popup)
(define-key projectile-key-map (kbd "C-v C-d") 'magit-diff-popup)
(define-key projectile-key-map (kbd "C-v C-g") 'gited-list-branches)
;; magit-blame-mode
(define-key projectile-key-map (kbd "C-v C-b")
  (lambda ()
    (interactive)
    (if magit-blame-mode
        (magit-blame-quit)
      (magit-blame-addition "-w"))))
(global-set-key (kbd "C-<prior>") nil)
(global-set-key (kbd "C-<next>") nil)

;; BUFFER LOCAL SEARCHING BINDS
(define-key search-key-map (kbd "C-r") 'vr/query-replace)
(define-key search-key-map (kbd "C-f") 'isearch-forward)
(define-key search-key-map (kbd "C-s") 'isearch-forward-symbol-at-point)
(define-key search-key-map (kbd "C-d") 'custom/isearch-forward-defun-name)
(define-key search-key-map (kbd "C-o") 'custom/isearch-to-occur)
(define-key search-key-map (kbd "C-g") 'rgrep)
(add-hook 'isearch-mode-hook 'custom/enhance-isearch)
(define-key query-replace-map (kbd "<return>") 'act)

;; MISC
(global-set-key (kbd "M-u") 'custom/toggle-camelcase-snakecase)

;; ELISP BINDS
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "M-*") 'helpful-at-point)
            (local-set-key (kbd "M--") 'elisp-refs-symbol)
            (local-set-key (kbd "C-.") 'find-function)
            (local-set-key (kbd "M-e M-s") 'eval-last-sexp)
            (local-set-key (kbd "M-e M-d") 'eval-defun)
            (define-prefix-command 'custom/edebug/prefix)
            (local-set-key (kbd "M-e M-e") 'custom/edebug/prefix)
            (define-key custom/edebug/prefix (kbd "M-d")
              'edebug-eval-top-level-form)
            (define-key custom/edebug/prefix (kbd "M-q")
              'custom/edebug-remove-instrumentation)
            (define-key custom/edebug/prefix (kbd "M-e") 'eval-expression)
            (local-set-key (kbd "M-e M-r") 'eval-region)
            (local-set-key (kbd "M-e M-b") 'eval-buffer)
            (local-set-key (kbd "M-e M-p") 'eval-print-last-sexp)
            (local-set-key (kbd "M-m") 'custom/elisp-macroexpand)
            (local-set-key (kbd "M-<next>") 'end-of-defun)
            (local-set-key (kbd "M-<prior>") 'beginning-of-defun)
            (local-set-key (kbd "M-d") 'custom/mark-defun)
            (local-set-key (kbd "M-a") 'custom/mark-args)
            (local-set-key (kbd "<tab>") 'company-indent-or-complete-common)
            (local-set-key (kbd "C-x C-<right>") 'forward-list)
            (local-set-key (kbd "C-x C-<left>") 'backward-list)
            (local-set-key (kbd "C-x C-<up>") 'up-list)
            (local-set-key (kbd "C-x C-<down>") 'down-list)
            (set (make-local-variable 'company-backends)
                 '((company-capf
                    company-dabbrev-code
                    company-files)))
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))
            (when (not (eq system-type 'windows-nt))
              (flyspell-prog-mode)
              (flycheck-mode))))
;; define M-. and M-, for jumping around elips sources (funs and vars)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
(define-key elisp-slime-nav-mode-map (kbd "M-,") 'custom/universal-pop-mark)


;; C BINDS
(add-hook 'c-mode-hook
          (lambda ()
            (setq comment-start "//"
                  comment-end   "")
            (local-set-key (kbd "C-d") 'custom/mark-whole-word)
            (local-set-key (kbd "M-.") 'cme-jump)
            (local-set-key (kbd "M-,") 'custom/universal-pop-mark)
            (local-set-key (kbd "M--") 'cme-symref)
            (local-set-key (kbd "M-*") 'cme-doc)
            (local-set-key (kbd "C-,") 'cme-proto-impl-toggle)
            (local-set-key (kbd "C-.") 'cme-find-anything)
            (local-set-key (kbd "C--") 'cme-rename-local-var)
            (local-set-key (kbd "M-<next>") 'cme-next-tag)
            (local-set-key (kbd "M-<prior>") 'cme-previous-tag)
            (local-set-key (kbd "M-p") 'cme-follow-ref-up)
            (define-prefix-command 'custom/semantic/transpose-map)
            (local-set-key (kbd "M-t") 'custom/semantic/transpose-map)
            (define-key custom/semantic/transpose-map
              (kbd "<down>") 'senator-transpose-tags-down)
            (define-key custom/semantic/transpose-map
              (kbd "<up>") 'senator-transpose-tags-up)
            (local-set-key (kbd "M-f") 'cme-fold-tag-toggle)
            (local-set-key (kbd "M-d") 'custom/mark-defun)
            (local-set-key (kbd "M-a") 'custom/mark-args)
            (local-set-key (kbd "M-g") 'cme-reparse-buffer)
            (local-set-key (kbd "<tab>") 'company-indent-or-complete-common)
            (local-set-key (kbd "C-x C-<right>") 'forward-list)
            (local-set-key (kbd "C-x C-<left>") 'backward-list)
            (local-set-key (kbd "C-x C-<up>") 'up-list)
            (local-set-key (kbd "C-x C-<down>") 'down-list)
            (c-toggle-auto-newline -1)
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))
            (when (not (eq system-type 'windows-nt))
              (flyspell-prog-mode))
            (flycheck-mode)))

;; C++ BINDS
(add-hook 'c++-mode-hook
          (lambda ()
            (setq comment-start "//"
                  comment-end   "")
            (local-set-key (kbd "C-d") 'custom/mark-whole-word)
            (local-set-key (kbd "M-.") 'cme-jump)
            (local-set-key (kbd "M-,") 'custom/universal-pop-mark)
            (local-set-key (kbd "M--") 'cme-symref)
            (local-set-key (kbd "M-*") 'cme-doc)
            (local-set-key (kbd "C-,") 'cme-proto-impl-toggle)
            (local-set-key (kbd "C-.") 'cme-find-anything)
            (local-set-key (kbd "C--") 'cme-rename-local-var)
            (local-set-key (kbd "M-<next>") 'cme-next-tag)
            (local-set-key (kbd "M-<prior>") 'cme-previous-tag)
            (local-set-key (kbd "M-p") 'cme-follow-ref-up)
            (local-set-key (kbd "M-c") 'cme-find-subclasses)
            (define-prefix-command 'custom/semantic/transpose-map)
            (local-set-key (kbd "M-t") 'custom/semantic/transpose-map)
            (define-key custom/semantic/transpose-map
              (kbd "<down>") 'senator-transpose-tags-down)
            (define-key custom/semantic/transpose-map
              (kbd "<up>") 'senator-transpose-tags-up)
            (local-set-key (kbd "M-f") 'cme-fold-tag-toggle)
            (local-set-key (kbd "M-d") 'custom/mark-defun)
            (local-set-key (kbd "M-a") 'custom/mark-args)
            (local-set-key (kbd "M-g") 'cme-reparse-buffer)
            (local-set-key (kbd "<tab>") 'company-indent-or-complete-common)
            (local-set-key (kbd "C-x C-<right>") 'forward-list)
            (local-set-key (kbd "C-x C-<left>") 'backward-list)
            (local-set-key (kbd "C-x C-<up>") 'up-list)
            (local-set-key (kbd "C-x C-<down>") 'down-list)
            (c-toggle-auto-newline -1)
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))
            (when (not (eq system-type 'windows-nt))
              (flyspell-prog-mode))
            (flycheck-mode)))

;; CMAKE BINDS
(add-hook 'cmake-mode-hook
          (lambda ()
            (local-set-key (kbd "C-d") 'custom/mark-whole-word)
            (local-set-key (kbd "M-*") 'cmake-help)
            (local-set-key (kbd "<tab>") 'company-indent-or-complete-common)
            (set (make-local-variable 'company-backends)
                 '((company-cmake
                    company-files)))
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))
            (when (not (eq system-type 'windows-nt))
              (flyspell-prog-mode))
            (flycheck-mode)))

;; HEXL BINDS
(add-hook 'hexl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-g") 'hexl-goto-hex-address)
            (local-set-key (kbd "C-w") 'custom/kill-buffer)))

;; NEOTREE BINDS
(define-key neotree-mode-map (kbd "n") 'neotree-create-node)
(define-key neotree-mode-map (kbd "d") 'neotree-delete-node)
(define-key neotree-mode-map (kbd "r") 'neotree-rename-node)
(define-key neotree-mode-map (kbd "c") 'neotree-copy-node)
(define-key neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
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
    ;; Critical fix, it is responsible for consistent catching of C-c.
    (define-key map (kbd "C-c") 'cua-copy-region)
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

;; Flycheck BINDS
(global-set-key (kbd "M-l") 'custom/lint-this-buffer)
