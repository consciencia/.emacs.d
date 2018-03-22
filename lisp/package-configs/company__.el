(custom/install-package-when-needed 'company)
(custom/install-package-when-needed 'company-c-headers)
(require 'company)
(require 'company-c-headers)

(add-hook 'after-init-hook 'global-company-mode)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.1
      company-search-regexp-function 'company-search-flex-regexp
      company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-idle-delay 0.1
      company-tooltip-limit 15
      company-dabbrev-downcase nil
      company-backends '((company-capf
                          company-files)
                         (company-semantic
                          company-c-headers)
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

(run-with-idle-timer 0.1 t
                     (lambda ()
                       (if (or (equal major-mode 'c-mode)
                               (equal major-mode 'c++-mode)
                               (equal major-mode 'emacs-lisp-mode))
                           (if (custom/in-comment)
                               (if company-mode (company-mode -1))
                             (if (not company-mode) (company-mode 1))))))
