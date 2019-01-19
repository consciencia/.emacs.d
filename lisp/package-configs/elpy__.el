(custom/install-package-when-needed 'elpy)
(require 'elpy)

(setq elpy-modules (delq 'elpy-module-company elpy-modules))
(setq elpy-rpc-backend "jedi")

(advice-add #'elpy-goto-location
            :after (lambda (&rest args)
                     (pulse-momentary-highlight-one-line (point))
                     (recenter)))

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(elpy-company-backend))))

(elpy-enable)



;; elpy-mode-map
(define-key elpy-mode-map (kbd "<C-down>")
  'elpy-nav-forward-block)
(define-key elpy-mode-map (kbd "<C-up>")
  'elpy-nav-backward-block)
(define-key elpy-mode-map (kbd "<C-left>")
  'elpy-nav-backward-indent)
(define-key elpy-mode-map (kbd "<C-right>")
  'elpy-nav-forward-indent)
(define-key elpy-mode-map (kbd "<tab>")
  'company-indent-or-complete-common)
(define-key elpy-mode-map (kbd "<M-down>") nil)
(define-key elpy-mode-map (kbd "<M-up>") nil)
(define-key elpy-mode-map (kbd "<M-left>") nil)
(define-key elpy-mode-map (kbd "<M-right>") nil)
(define-key elpy-mode-map (kbd "M-<next>")
  'python-nav-forward-defun)
(define-key elpy-mode-map (kbd "M-<prior>")
  'python-nav-backward-defun)
(define-key elpy-mode-map (kbd "M-f") nil)
(define-key elpy-mode-map (kbd "M-*") 'elpy-doc)
(define-key elpy-mode-map (kbd "M-d")
        (lambda ()
          (interactive)
          (call-interactively 'python-mark-defun)
          (setq transient-mark-mode (cons 'only transient-mark-mode))))
(define-key elpy-mode-map (kbd "C-<right>") 'custom/forward-symbol)
(define-key elpy-mode-map (kbd "C-<left>") 'custom/backward-symbol)
(define-key elpy-mode-map (kbd "C-<down>") 'forward-paragraph)
(define-key elpy-mode-map (kbd "C-<up>") 'backward-paragraph)
(define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition)
(define-key elpy-mode-map (kbd "M-,") (lambda ()
                                        (interactive)
                                        (xref-pop-marker-stack)
                                        (pulse-momentary-highlight-one-line
                                         (point))
                                        (recenter)))
