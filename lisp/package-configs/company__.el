(custom/install-package-when-needed 'company)
(custom/install-package-when-needed 'company-c-headers)
(custom/install-package-when-needed 'company-web)
(require 'company)
(require 'company-c-headers)
(require 'company-web-html)

(add-hook 'after-init-hook 'global-company-mode)

(setq company-minimum-prefix-length 1
      company-idle-delay 2
      company-search-regexp-function 'company-search-flex-regexp
      company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-idle-delay 2
      company-tooltip-limit 10
      company-dabbrev-downcase nil
      company-backends '((company-capf
                          company-files)
                         (company-semantic
                          company-c-headers)
                         (elpy-company-backend
                          company-files)
                         (company-etags
                          company-dabbrev-code
                          company-keywords)
                         company-css
                         company-nxml
                         company-cmake
                         company-files
                         company-xcode
                         company-bbdb
                         company-oddmuse
                         company-dabbrev)
      company-c-headers-path-system (lambda ()
                                      (when ede-object
                                        (ede-system-include-path ede-object))))
(setq company-dabbrev-ignore-case nil)

;; (run-with-idle-timer 0.1 t
;;                      (lambda ()
;;                        (if (custom/pos-is-in-comment)
;;                            (if company-mode (company-mode -1))
;;                          (if (not company-mode)
;;                              (company-mode 1)))))
(setq *custom/company-backends-backup* nil)
(run-with-idle-timer 0.1 t
                     (lambda ()
                       (if (custom/pos-is-in-comment)
                           (when (null *custom/company-backends-backup*)
                             (message "Storing old company backends %s"
                                      company-backends)
                             (setq *custom/company-backends-backup*
                                   company-backends)
                             (setq company-backends
                                   '(company-dabbrev)))
                         (when *custom/company-backends-backup*
                           (message "Restoring old company backends %s"
                                      *custom/company-backends-backup*)
                           (setq company-backends
                                 *custom/company-backends-backup*)
                           (setq *custom/company-backends-backup* nil)))))



(define-key company-active-map (kbd "M-*") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-search-candidates)
(define-key company-search-map (kbd "<prior>") 'company-search-repeat-backward)
(define-key company-search-map (kbd "<next>") 'company-search-repeat-forward)
(define-key company-search-map (kbd "<tab>") 'company-search-abort)
(global-set-key (kbd "C-SPC") 'company-complete-common)
