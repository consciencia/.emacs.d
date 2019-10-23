(custom/install-package-when-needed 'ace-jump-mode)
(custom/install-package-when-needed 'avy)
(require 'ace-jump-mode)
(require 'avy)

(setq ace-jump-mode-case-fold t
      ace-jump-mode-move-keys (loop for i from ?a to ?z collect i)
      ace-jump-word-mode-use-query-char t
      ace-jump-mode-scope 'window
      avy-background nil)
