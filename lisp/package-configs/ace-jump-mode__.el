(custom/install-package-when-needed 'ace-jump-mode)
(require 'ace-jump-mode)

(setq ace-jump-mode-case-fold t
      ace-jump-mode-move-keys (loop for i from ?a to ?z collect i)
      ace-jump-word-mode-use-query-char nil
      ace-jump-mode-scope 'window)
