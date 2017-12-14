(custom/install-package-when-needed 'idle-highlight-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (idle-highlight-mode t)))
(setq idle-highlight-idle-time 1)
