(custom/install-package-when-needed 'ws-butler)

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)
