(custom/install-package-when-needed 'smart-mode-line)
(custom/install-package-when-needed 'smart-mode-line-powerline-theme)

(setq sml/name-width 60
      sml/mode-width 'full
      sml/no-confirm-load-theme t
      sml/theme nil)

(sml/setup)
(sml/apply-theme 'powerline)

(custom-set-faces
 `(sml/prefix ((t :foreground "lightgray")))
 `(sml/folder ((t :foreground "lightgray")))
 `(sml/position-percentage ((t :foreground "lightgray")))
 `(sml/vc ((t :foreground "lightgray")))
 `(sml/vc-edited ((t :foreground "gold")))
 `(sml/modes ((t :foreground "lightgray"))))

(setq mode-line-modes
      (loop for desc in mode-line-modes
            if (not (equal desc '(:eval (sml/generate-minor-modes))))
            collect desc))

;; TODO:
;; Try modeline from doom mode.
;; https://github.com/seagle0128/doom-modeline
