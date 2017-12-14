(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-clang company-xcode company-cmake company-capf company-files
                  (company-gtags company-etags company-keywords company-semantic)
                  company-oddmuse company-dabbrev)))
 '(company-idle-delay 0.1)
 '(company-show-numbers t)
 '(company-tooltip-idle-delay 0.1)
 '(custom-safe-themes
   (quote
    ("b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(ggtags-auto-jump-to-match nil)
 '(global-semantic-stickyfunc-mode t)
 '(hexl-bits 8)
 '(jit-lock-stealth-nice 0)
 '(minimap-automatically-delete-window nil)
 '(minimap-major-modes (quote (prog-mode)))
 '(minimap-minimum-width 20)
 '(minimap-window-location (quote right))
 '(neo-create-file-auto-open t)
 '(neo-show-hidden-files t)
 '(semantic-default-submodes
   (quote
    (global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-highlight-edits-mode)))
 '(semantic-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-foreground ((t (:foreground "white" :underline (:color "lightblue" :style wave))))))
