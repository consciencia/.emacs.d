(custom/install-package-when-needed 'neotree)
(require 'neotree)

(setq neo-smart-open t
      neo-theme 'ascii
      neo-confirm-change-root 'off-p
      projectile-switch-project-action 'custom/projectile-switch-proj-action)

(if (daemonp)
    (add-hook 'server-switch-hook 'custom/neotree-startup)
  (add-hook 'after-init-hook 'custom/neotree-startup))

(add-hook 'custom/after-switch-to-buffer-hook
          'custom/neotree/reveal-file)

(add-hook 'custom/after-select-buffer-hook
          'custom/neotree/reveal-file)

