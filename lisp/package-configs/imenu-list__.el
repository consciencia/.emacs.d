(custom/install-package-when-needed 'imenu-list)

(setq imenu-list-position 'right)
(setq imenu-list-size 0.30)
(setq imenu-list-focus-after-activation nil)

(imenu-list-minor-mode)

(add-hook 'after-make-frame-functions
          #'custom/create-imenu-list)

