(custom/install-package-when-needed 'neotree)
(require 'neotree)

(setq neo-smart-open t)
(setq neo-theme 'ascii)

(setq projectile-switch-project-action
      (lambda (&rest args)
        (let* ((projectile-completion-system #'completing-read-default))
          (apply 'projectile-find-file args))
        (apply 'neotree-projectile-action args)
        (call-interactively 'windmove-right)
        (neotree-find)
        (call-interactively 'windmove-right)))

(defun neotree-startup ()
  (interactive)
  (neotree-show)
  (call-interactively 'other-window))

(if (daemonp)
    (add-hook 'server-switch-hook #'neotree-startup)
  (add-hook 'after-init-hook #'neotree-startup))
