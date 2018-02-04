(custom/install-package-when-needed 'neotree)
(require 'neotree)

(setq neo-smart-open t)
(setq neo-theme 'ascii)

(setq projectile-switch-project-action
      (lambda (&rest args)
        (custom/project/load-loader (projectile-project-root))
        (let* ((projectile-completion-system #'custom/default-completing-read))
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
