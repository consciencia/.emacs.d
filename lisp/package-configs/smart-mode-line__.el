(custom/install-package-when-needed 'smart-mode-line) 
(custom/install-package-when-needed 'smart-mode-line-powerline-theme)

(setq sml/name-width 60
      sml/mode-width 'full
      sml/no-confirm-load-theme t
      sml/theme nil)

(sml/setup)
