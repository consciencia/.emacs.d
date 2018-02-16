(defvar custom/after-switch-to-buffer-hook nil)
(advice-add 'switch-to-buffer :after 
            (lambda (&rest args)
              (run-hooks 'custom/after-switch-to-buffer-hook)))

(defvar custom/after-select-buffer-hook nil)
(advice-add 'windmove-do-window-select :after 
            (lambda (&rest args)
              (run-hooks 'custom/after-select-buffer-hook)))

(defvar custom/buffname-before-window-select nil)
(advice-add 'windmove-do-window-select :before 
            (lambda (&rest args)
              (setq custom/buffname-before-window-select (buffer-name))))
