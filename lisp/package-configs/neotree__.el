(custom/install-package-when-needed 'neotree)
(require 'neotree)

(setq neo-smart-open t
      neo-theme 'ascii
      neo-confirm-change-root 'off-p
      neo-autorefresh nil
      neo-create-file-auto-open nil
      neo-show-hidden-files t)

(if (daemonp)
    (add-hook 'server-switch-hook 'custom/neotree-startup)
  (add-hook 'after-init-hook 'custom/neotree-startup))

(setq custom/neotree/last-buffer nil)
(run-with-idle-timer 0.5 t
                     (lambda ()
                       (ignore-errors
                         (let* ((buff-name-raw (buffer-name))
                                (buff-name (if (stringp buff-name-raw) buff-name-raw "  ")))
                           (if (not (or (string= (substring buff-name 0 1) "*")
                                        (string= (substring buff-name 0 2) " *")
                                        (string= buff-name custom/neotree/last-buffer)
                                        (not (neo-global--window-exists-p))))
                               (progn
                                 (setq custom/neotree/last-buffer buff-name)
                                 (let ((proj-root (custom/get-project-root)))
                                   (if (and (stringp proj-root)
                                            (not (equal (expand-file-name
                                                         (substitute-in-file-name proj-root))
                                                        neo-buffer--start-node)))
                                       (progn
                                         (push-mark nil t nil)
                                         (neotree-dir proj-root)
                                         (pop-global-mark))))))))))
