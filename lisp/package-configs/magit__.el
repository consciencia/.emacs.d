(custom/install-package-when-needed 'magit)
(custom/install-package-when-needed 'gited)
(require 'magit)
(require 'gited)

(setq magit-completing-read-function
      'magit-ido-completing-read)

(define-key smerge-mode-map (kbd "C-<next>") 'smerge-next)
(define-key smerge-mode-map (kbd "C-<prior>") 'smerge-prev)

(run-with-idle-timer 5 t #'custom/try-to-use-smerge)

(if (boundp 'transient-base-map)
    (progn
      (define-key transient-base-map (kbd "q") 'transient-quit-one)))

;; Needed because CUA override in emulation-mode-map-alists failed.
;; What the fuck omg.
(loop for m in (list magit-diff-mode-map
                     magit-file-section-map
                     magit-hunk-section-map
                     magit-unstaged-section-map
                     magit-staged-section-map)
      do (define-key m (kbd "C-c") 'cua-copy-region))
