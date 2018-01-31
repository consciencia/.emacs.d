(custom/install-package-when-needed 'imenu-list)
(setq imenu-list-position 'right)
(setq imenu-list-size 0.30)
(setq imenu-list-focus-after-activation nil)
(imenu-list-minor-mode)

(defun custom/new-create-imenu-list (new-f)
  (select-frame new-f)
  (imenu-list-show)
  (other-window 1))

(setq new-frame-cache nil)
(defun custom/old-create-imenu-list (new-f)
  (setq new-frame-cache new-f)
  (run-at-time "1 sec" nil
               (lambda ()
                 (select-frame new-frame-cache)
                 (imenu-list-show)
                 (other-window 1))))

(add-hook 'after-make-frame-functions
          (lambda (new-f)
            (if (< emacs-major-version 25)
                (progn
                  (custom/old-create-imenu-list new-f))
              (progn
                (custom/old-create-imenu-list new-f)))))

