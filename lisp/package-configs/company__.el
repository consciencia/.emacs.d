(custom/install-package-when-needed 'company)
(custom/install-package-when-needed 'company-c-headers)
(custom/install-package-when-needed 'company-web)
(custom/install-package-when-needed 'company-statistics)
(require 'company)
(require 'company-c-headers)
(require 'company-web-html)
(require 'company-statistics)
(require 'semantic/dep)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'company-statistics-mode)

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
                         (company-c-headers
                          company-semantic)
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
                         company-dabbrev))
(setq company-c-headers-path-system
      (lambda ()
        (when ede-object
          (append (ede-system-include-path ede-object)
                  semantic-dependency-system-include-path
                  (loop for inc in (ede-include-path ede-object)
                        collect (s-replace "//" "/"
                                           (s-concat (projectile-project-root)
                                                     inc)))))))
(setq company-dabbrev-ignore-case nil)



(define-key company-active-map (kbd "M-*") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-search-candidates)
(define-key company-search-map (kbd "<prior>") 'company-search-repeat-backward)
(define-key company-search-map (kbd "<next>") 'company-search-repeat-forward)
(define-key company-search-map (kbd "<tab>") 'company-search-abort)



(defun custom/company-semantic-completions-raw (prefix)
  (setq company-semantic--current-tags nil)
  (dolist (tag (if (and (fboundp 'semanticdb-minor-mode-p)
                        (semanticdb-minor-mode-p))
                   ;; Search the database & concatenate all matches together.
                   (semanticdb-fast-strip-find-results
                    (semanticdb-find-tags-for-completion prefix))
                 ;; Search just this file because there is no DB available.
                 (semantic-find-tags-for-completion
                  prefix (current-buffer))))
    (unless (eq (semantic-tag-class tag) 'include)
      (push tag company-semantic--current-tags)))
  (delete "" (mapcar 'semantic-tag-name company-semantic--current-tags)))

(advice-add #'company-semantic-completions-raw
            :around
            (lambda (oldfn &rest args)
              (apply #'custom/company-semantic-completions-raw args)))
