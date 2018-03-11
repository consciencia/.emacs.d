(load "custom-paredit.el")
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



(defun custom-paredit-init ()
  (local-set-key (kbd "(" ) 'paredit-open-round) 
  (local-set-key (kbd ")" ) 'paredit-close-round)
  (local-set-key (kbd "[" ) 'paredit-open-square)
  (local-set-key (kbd "]" ) 'paredit-close-square) 
  (local-set-key (kbd "\"" ) 'paredit-doublequote)
  (local-set-key (kbd ";" ) 'paredit-semicolon)
  (local-set-key (kbd "C-<right>") 'paredit-forward)
  (local-set-key (kbd "C-<left>") 'paredit-backward)
  (local-set-key (kbd "C-<down>") 'paredit-forward-down)
  (local-set-key (kbd "C-<up>") 'paredit-backward-up))
