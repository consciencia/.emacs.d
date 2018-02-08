;; (custom/install-package-when-needed 'ggtags)

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode
;;                                   'c++-mode
;;                                   'java-mode
;;                                   'asm-mode)
;;               (ggtags-mode 1))))

;; (setq ggtags-completing-read-function
;;       'ido-completing-read)

;; (add-hook 'ggtags-global-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-<down>") 'compilation-next-error)
;;             (local-set-key (kbd "C-<up>") 'compilation-previous-error)
;;             (local-set-key (kbd "C-<right>") 'compilation-next-file)
;;             (local-set-key (kbd "C-<left>") 'compilation-previous-file)))

;; (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
