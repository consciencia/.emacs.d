(custom/install-package-when-needed 'idle-highlight-mode)
(require 'idle-highlight-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (idle-highlight-mode t)))

(setq idle-highlight-idle-time 1)

(advice-add #'idle-highlight-word-at-point
            :around (lambda (oldfn &rest args)
                      (ignore-errors
                        (apply oldfn args))))
