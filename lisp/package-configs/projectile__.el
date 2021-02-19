(custom/install-package-when-needed 'projectile)
(require 'projectile)
(require 'cl)

(setq projectile-tags-backend 'auto
      projectile-track-known-projects-automatically nil
      projectile-switch-project-action 'projectile-find-file)

(projectile-mode)

(cl-defun projectile-completing-read (prompt choices &key initial-input action)
  "Present a project tailored PROMPT with CHOICES."
  (let ((prompt (projectile-prepend-project-name prompt))
        res)
    (setq res
          (cond
           ((<= (length choices) 5000)
            (ido-completing-read prompt choices nil nil initial-input))
           ((> (length choices) 5000)
            (ivy-read prompt choices
                      :initial-input initial-input
                      :action (prog1 action
                                (setq action nil))
                      :caller 'projectile-completing-read))))
    (if action
        (funcall action res)
      res)))
