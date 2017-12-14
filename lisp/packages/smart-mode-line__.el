(custom/install-package-when-needed 'smart-mode-line) 
(custom/install-package-when-needed 'smart-mode-line-powerline-theme)
(setq sml/no-confirm-load-theme t)
;; can not use powerline because light version is not in MELPA
;; and dark version cant be seen well when using dark theme
;; (setq sml/theme 'powerline)
(setq sml/theme 'light)
(setq sml/name-width 60)
(setq sml/mode-width 'full)
(sml/setup)
