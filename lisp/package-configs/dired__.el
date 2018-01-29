(custom/install-package-when-needed 'dired-imenu)
(require 'dired-imenu)
(load "dired+.el")
(require 'dired+)

(setq diredp-bind-problematic-terminal-keys nil)
(diredp-toggle-find-file-reuse-dir 1)

(eval-after-load "dired-aux"
 '(add-to-list 'dired-compress-file-suffixes 
               '("\\.zip\\'" ".zip" "unzip")))

(add-hook 'dired-load-hook
         (lambda ()
         	(load "dired-x")))


