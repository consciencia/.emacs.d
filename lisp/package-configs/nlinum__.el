(custom-set-faces
 `(line-number ((t (:foreground "white"
                    :background ,(face-attribute 'default
                                                 :background)))))
 `(line-number-current-line ((t (:foreground "red"
                                 :background ,(face-attribute 'default
                                                              :background))))))

(when (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(when (not (version<= "26.0.50" emacs-version))
  (custom/install-package-when-needed 'nlinum)
  (require 'nlinum)

  (add-hook 'prog-mode-hook 'nlinum-mode))
