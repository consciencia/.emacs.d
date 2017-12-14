(custom/install-package-when-needed 'dired+)
(setq diredp-bind-problematic-terminal-keys nil)
(require 'dired+)
(add-hook 'dired-load-hook
          (lambda ()
          	(load "dired-x")))
(diredp-toggle-find-file-reuse-dir 1)
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes 
                '("\\.zip\\'" ".zip" "unzip")))
