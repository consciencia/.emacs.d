(custom/install-package-when-needed 'imenu-list)
(setq imenu-list-position 'right)
(setq imenu-list-size 0.25)
(setq imenu-list-focus-after-activation nil)
(imenu-list-minor-mode)
(add-hook 'after-make-frame-functions
          (lambda (new-f)
            (select-frame new-f)
            (imenu-list-show)))
