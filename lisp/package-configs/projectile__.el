(custom/install-package-when-needed 'projectile)

(setq projectile-tags-backend 'ggtags)
(setq projectile-track-known-projects-automatically nil)

(projectile-mode)

(defun custom/projectile-switch-proj-action (&rest args)
  (let* ((projectile-completion-system
          #'custom/default-completing-read))
    (apply 'projectile-find-file args)))

(setq projectile-switch-project-action 'custom/projectile-switch-proj-action)
