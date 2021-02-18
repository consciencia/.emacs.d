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

(setq *custom/proj-root-cache* (custom/map/create))
(defun custom/projectile-project-root (oldfn &rest args)
  (let ((path (buffer-file-name (current-buffer)))
        root)
    (if (and path
             (@in *custom/proj-root-cache* path))
        (custom/map/get path *custom/proj-root-cache*)
      (progn
        (setq root (apply oldfn args))
        (when path
          (custom/map/set path root *custom/proj-root-cache*))
        root))))

(advice-add #'projectile-project-root :around 'custom/projectile-project-root)
