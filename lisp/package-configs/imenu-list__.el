(require 'imenu)
(custom/install-package-when-needed 'imenu-list)
(require 'imenu-list)

(setq-default imenu-list-position 'right)
(setq-default imenu-list-size 0.25)
(setq-default imenu-list-auto-resize nil)
(setq-default imenu-list-focus-after-activation nil)

(imenu-list-minor-mode)

;; make sure visual line mode is active
(if (functionp 'global-visual-line-mode)
    (if (not global-visual-line-mode)
        (global-visual-line-mode 1)))

;; make imenu in every new frame
;; this is questionable feature, maybe
;; it will end removed
(add-hook 'after-make-frame-functions
          #'custom/create-imenu-list)

(advice-add #'imenu-list-update
            :around
            (lambda (oldfun &rest args)
              (let ((old (current-buffer))
                    (im (get-buffer imenu-list-buffer-name)))
                (when (and (not (active-minibuffer-window))
                           im)
                  (switch-to-buffer-other-window im)
                  (switch-to-buffer-other-window old)))
              (when (not (custom/pos-is-in-comment))
                (apply oldfun args))))
