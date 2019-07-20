(custom/install-package-when-needed 'multi-term)
(require 'multi-term)

(setq multi-term-program "/bin/bash")


(push (cons "C-S-c" 'cua-copy-region)
      term-bind-key-alist)
(push (cons "C-S-v" 'cua-paste)
      term-bind-key-alist)
(push (cons "C-w" 'kill-buffer)
      term-bind-key-alist)
