(custom/install-package-when-needed 'highlight-indentation)
(require 'highlight-indentation)

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (highlight-indentation-mode t)
            (set-face-background 'highlight-indentation-face
                                 "#333333")))
