(custom/install-package-when-needed 'smart-mode-line)
(custom/install-package-when-needed 'smart-mode-line-powerline-theme)

(setq sml/name-width 60
      sml/mode-width 'full
      sml/no-confirm-load-theme t
      sml/theme nil)

(sml/setup)

(setq mode-line-modes
      (loop for desc in mode-line-modes
            if (not (equal desc '(:eval (sml/generate-minor-modes))))
            collect desc))
