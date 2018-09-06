(custom/install-package-when-needed 'highlight-indentation)
(require 'highlight-indentation)

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (highlight-indentation-mode t)
            (set-face-background 'highlight-indentation-face
                                 "#555555")
            (set-face-background 'highlight-indentation-current-column-face
                                 "#888888")))
