(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-xcode company-cmake company-semantic company-capf company-files company-oddmuse company-dabbrev)))
 '(company-idle-delay 0.1)
 '(company-search-regexp-function (quote company-search-flex-regexp))
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(company-tooltip-idle-delay 0.1)
 '(company-tooltip-limit 15)
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(ecb-layout-name "leftright2")
 '(ecb-new-ecb-frame t)
 '(ecb-options-version "2.50")
 '(ecb-tip-of-the-day nil)
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  ")
 '(git-gutter:update-interval 1)
 '(hexl-bits 8)
 '(imenu-list-auto-resize t)
 '(imenu-list-size 0.2)
 '(jit-lock-stealth-nice 0)
 '(mc/always-repeat-command t)
 '(mc/edit-lines-empty-lines (quote ignore))
 '(minimap-automatically-delete-window nil)
 '(minimap-major-modes (quote (prog-mode)))
 '(minimap-minimum-width 20)
 '(minimap-window-location (quote right))
 '(neo-autorefresh nil)
 '(neo-create-file-auto-open nil)
 '(neo-show-hidden-files t)
 '(package-selected-packages
   (quote
    (pcre2el git-gutter adaptive-wrap magit multiple-cursors shackle stickyfunc-enhance eldoc-overlay visual-regexp-steroids visual-regexp imenu-list company ggtags undo-tree smart-mode-line-powerline-theme smart-mode-line projectile neotree minimap ido-completing-read+ flx-ido ido-vertical-mode idle-highlight-mode f elisp-slime-nav dired+ autopair ace-window ace-jump-mode spacemacs-theme)))
 '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-tooltip))
 '(semantic-edits-verbose-flag t)
 '(speedbar-use-images nil)
 '(speedbar-use-imenu-flag nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-foreground ((t (:foreground "white" :underline (:color "lightblue" :style wave)))))
 '(ecb-default-highlight-face ((t (:background "white" :foreground "black"))))
 '(whitespace-line ((t (:foreground "red")))))
