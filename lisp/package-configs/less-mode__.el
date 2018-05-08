(custom/install-package-when-needed 'less-css-mode)
(require 'less-css-mode)

(add-hook 'less-css-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-css))
            (local-set-key (kbd "<tab>") 'company-indent-or-complete-common)))
