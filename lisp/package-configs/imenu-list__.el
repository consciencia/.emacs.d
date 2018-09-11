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

(defun custom|imenu-list-update (oldfun &rest args)
  (let* ((old (current-buffer))
         (oldwin (get-buffer-window old))
         (im (get-buffer imenu-list-buffer-name))
         (win (if im (get-buffer-window im))))
    (when (and (not (active-minibuffer-window))
               im win)
      (switch-to-buffer-other-window im)
      (select-window oldwin)
      (switch-to-buffer old))
    (apply oldfun args)))

(advice-add #'imenu-list-update
            :around 'custom|imenu-list-update)
