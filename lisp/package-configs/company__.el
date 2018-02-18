(custom/install-package-when-needed 'company)

(add-hook 'after-init-hook 'global-company-mode)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.1
      company-search-regexp-function 'company-search-flex-regexp
      company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-idle-delay 0.1
      company-tooltip-limit 15
      company-backends '(company-bbdb
                         company-nxml
                         company-css
                         company-eclim
                         company-xcode
                         company-cmake
                         company-semantic
                         company-capf
                         company-files
                         company-oddmuse
                         company-dabbrev))


