(custom/install-package-when-needed 'magit)
(require 'magit)

(setq magit-completing-read-function
      'magit-ido-completing-read)

(define-key smerge-mode-map (kbd "C-<next>") 'smerge-next)
(define-key smerge-mode-map (kbd "C-<prior>") 'smerge-prev)

(run-with-idle-timer 5 t #'custom/try-to-use-smerge)
