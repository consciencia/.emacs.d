(custom/install-package-when-needed 'web-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(setq web-mode-markup-indent-offset 4
      web-mode-css-indent-offset 4
      web-mode-code-indent-offset 4
      web-mode-style-padding 4
      web-mode-script-padding 4
      web-mode-enable-auto-pairing t
      web-mode-enable-css-colorization t
      web-mode-enable-block-face t
      web-mode-enable-current-element-highlight t
      web-mode-enable-current-column-highlight t)

(define-key web-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
(define-key web-mode-map (kbd "C-<down>") 'web-mode-element-next)
(define-key web-mode-map (kbd "C-<up>") 'web-mode-element-previous)
(define-key web-mode-map (kbd "M-r") 'web-mode-element-rename)
(define-key web-mode-map (kbd "M-d")
  (lambda ()
    (interactive)
    (web-mode-element-select)
    (setq transient-mark-mode (cons 'only transient-mark-mode))))

(add-hook 'web-mode-hook (lambda ()
                           (set (make-local-variable 'company-backends)
                                '((company-web-html
                                   company-css)))
                          (company-mode t)))
