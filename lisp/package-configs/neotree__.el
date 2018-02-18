(custom/install-package-when-needed 'neotree)
(require 'neotree)

(defun custom/projectile-switch-proj-action (&rest args)
  (let* ((projectile-completion-system
          #'custom/default-completing-read))
    (apply 'projectile-find-file args)))

(setq neo-smart-open t
      neo-theme 'ascii
      neo-confirm-change-root 'off-p
      projectile-switch-project-action 'custom/projectile-switch-proj-action
      neo-autorefresh nil
      neo-create-file-auto-open nil
      neo-show-hidden-files t)

(if (daemonp)
    (add-hook 'server-switch-hook 'custom/neotree-startup)
  (add-hook 'after-init-hook 'custom/neotree-startup))



(defvar custom/buffname-before-window-select nil)
(advice-add 'windmove-do-window-select :before 
            (lambda (&rest args)
              (setq custom/buffname-before-window-select (buffer-name))))

(advice-add 'windmove-do-window-select :after 
            (lambda (&rest args)
              (custom/neotree/reveal-file)))

(advice-add 'switch-to-buffer :after 
            (lambda (&rest args)
              (custom/neotree/reveal-file)))

(advice-add 'delete-window :after 
            (lambda (&rest args)
              (custom/neotree/reveal-file-force)))

(setq custom/neotree/reveal-file--ignore-next nil)
(defun custom/neotree/reveal-file ()
  (if (not (or (string= (substring (buffer-name) 0 1) "*")
               (string= (substring (buffer-name) 0 2) " *")
               custom/neotree/reveal-file--ignore-next
               (equal (buffer-name) custom/buffname-before-window-select)
               (not (neo-global--window-exists-p))))
      (progn
        (let ((proj-root projectile-cached-project-root)
              (old-buff (buffer-name)))
          (if (and proj-root
                   (not (equal (expand-file-name (substitute-in-file-name proj-root))
                               neo-buffer--start-node)))
              (neotree-dir proj-root))
          (setq custom/neotree/reveal-file--ignore-next t)
          (switch-to-buffer old-buff)))
    (if custom/neotree/reveal-file--ignore-next
        (setq custom/neotree/reveal-file--ignore-next nil))))

(defun custom/neotree/reveal-file-force ()
  (setq custom/buffname-before-window-select nil)
  (custom/neotree/reveal-file))




