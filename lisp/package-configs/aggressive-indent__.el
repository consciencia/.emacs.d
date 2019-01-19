(custom/install-package-when-needed 'aggressive-indent)
(require 'aggressive-indent)



(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'html-mode-hook #'aggressive-indent-mode)
(add-hook 'js-mode-hook #'aggressive-indent-mode)
(add-hook 'c-mode-hook #'aggressive-indent-mode)
(add-hook 'c++-mode-hook #'aggressive-indent-mode)
