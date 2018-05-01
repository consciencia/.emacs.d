(custom/install-package-when-needed 'js2-mode)
(require 'js2-mode)

(add-to-list 'auto-mode-alist
             '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist
             '("node" . js2-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)
