(require 'imenu)
(custom/install-package-when-needed 'imenu-list)
(require 'imenu-list)

(setq imenu-list-position 'right
      imenu-list-size 0.25
      imenu-list-auto-resize nil
      imenu-list-focus-after-activation nil)

(imenu-list-minor-mode)

(add-hook 'after-make-frame-functions
          #'custom/create-imenu-list)
