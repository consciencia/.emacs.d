(setq custom/tern-emacs-dir
      (concat user-emacs-directory
              "tern/tern/emacs/"))
(if (file-directory-p custom/tern-emacs-dir)
    (progn
      (add-to-list 'load-path
                   custom/tern-emacs-dir)
      (autoload 'tern-mode "tern.el" nil t)
      (require 'tern)

      (custom/install-package-when-needed 'company-tern)
      (require 'company-tern)
      (setq company-tern-property-marker " <p>")

      (add-hook 'js-mode-hook
                (lambda ()
                  (set (make-local-variable 'company-backends)
                       '(company-tern
                         company-files))
                  (tern-mode t)))

      (tern-mode)

      (advice-add #'tern-go-to-position
            :after (lambda (&rest args)
                     (pulse-momentary-highlight-one-line (point))
                     (recenter)
                     (js2-reparse nil)))

      (define-key tern-mode-keymap (kbd "M-.") 'tern-find-definition)
      (define-key tern-mode-keymap (kbd "M-,") 'tern-pop-find-definition)
      (define-key tern-mode-keymap (kbd "M-*") 'tern-get-docs)
      (define-key tern-mode-keymap (kbd "M-d")
        (lambda ()
          (interactive)
          (call-interactively 'js2-mark-defun)
          (setq transient-mark-mode (cons 'only transient-mark-mode))))
      (define-key tern-mode-keymap (kbd "<tab>") 'company-indent-or-complete-common)))



(defun custom/tern/generare-generic-loader (proj-root)
  (let ((proj-name (read-string "Project name: "
                                (let ((fragments (s-split custom/fs-separator
                                                          (file-name-as-directory proj-root))))
                                  (nth (- (length fragments) 2)
                                       fragments)))))
    `((custom/tern/load-project ,proj-name
                               ,(file-name-as-directory proj-root)))))

(defun custom/tern/load-project (proj-name proj-root)
  (custom/tern/generate-config-file proj-root))

(defun custom/tern/generate-config-file (proj-root)
  (if (not (file-exists-p (concat proj-root
                                  ".tern-project")))
      (f-write-text (concat "{\n"
                            "\t\"plugins\": {"
                            "\n\t\t\"node\": {},"
                            "\n\t\t\"modules\": {"
                            "\n\t\t\t\"load\": \".*\""
                            "\n\t\t},"
                            "\n\t\t\"es_modules\": {},"
                            "\n\t\t\"requirejs\": {"
                            "\n\t\t\t\"baseURL\": \"./\","
                            "\n\t\t\t\"paths\": {}"
                            "\n\t\t}"
                            "\n\t},\n"
                            "\t\"libs\": ["
                            "\n\t\t\"ecmascript\","
                            "\n\t\t\"browser\","
                            "\n\t\t\"underscore\","
                            "\n\t\t\"react\","
                            "\n\t\t\"jquery\""
                            "\n\t]"
                            "\n}")
                    'utf-8
                    (concat proj-root
                            ".tern-project"))))
