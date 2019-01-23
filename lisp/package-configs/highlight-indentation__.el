(custom/install-package-when-needed 'highlight-indentation)
(require 'highlight-indentation)

(setq highlight-indentation-hooks
      (loop for x in highlight-indentation-hooks
            if (not (equal (car x) 'window-scroll-functions))
            collect x))

(run-with-idle-timer 0.5 t
                     (lambda ()
                       (when highlight-indentation-mode
                         (highlight-indentation-redraw-window
                          (selected-window)
                          'highlight-indentation-overlay
                          'highlight-indentation-put-overlays-region))))

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (highlight-indentation-mode t)
            (set-face-background 'highlight-indentation-face
                                 "#333333")))
