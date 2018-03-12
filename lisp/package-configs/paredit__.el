(custom/install-package-when-needed 'paredit)
(require 'paredit)

(add-hook 'emacs-lisp-mode-hook
          #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook
          #'enable-paredit-mode)
(add-hook 'ielm-mode-hook
          #'enable-paredit-mode)
(add-hook 'lisp-mode-hook
          #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook
          #'enable-paredit-mode)
(add-hook 'scheme-mode-hook
          #'enable-paredit-mode)

(setf (cdr paredit-mode-map) nil)
(define-key paredit-mode-map (kbd "(" ) 'paredit-open-round) 
(define-key paredit-mode-map (kbd ")" ) 'paredit-close-round)
(define-key paredit-mode-map (kbd "[" ) 'paredit-open-square)
(define-key paredit-mode-map (kbd "]" ) 'paredit-close-square) 
(define-key paredit-mode-map (kbd "\"" ) 'paredit-doublequote)
(define-key paredit-mode-map (kbd ";" ) 'paredit-semicolon)
(define-key paredit-mode-map (kbd "C-<right>") 'paredit-forward)
(define-key paredit-mode-map (kbd "C-<left>") 'paredit-backward)
(define-key paredit-mode-map (kbd "C-<down>") 'paredit-forward-down)
(define-key paredit-mode-map (kbd "C-<up>") 'paredit-backward-up)
