(custom/install-package-when-needed 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))
(setq ggtags-completing-read-function
      'ido-completing-read)
(if (string-equal system-type "windows-nt")
    (setq ggtags-executable-directory
          "~/.emacs.d/ext-apps/gnu_global/bin/"))
