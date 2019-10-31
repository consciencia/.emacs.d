(custom/install-package-when-needed 'tern)
(custom/install-package-when-needed 'company-tern)
(require 'tern)
(require 'company-tern)

(setq company-tern-property-marker " <p>")

(tern-mode)

(add-hook 'js-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-tern
                   company-files))
            (tern-mode t)
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))
            (when (not (eq system-type 'windows-nt))
              (flyspell-prog-mode))))

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
;; js2-narrow-to-defun
;; extract info from js2 system and implement following features
;; ...
;; (define-key tern-mode-keymap (kbd "M-<next>") 'senator-next-tag)
;; (define-key tern-mode-keymap (kbd "M-<prior>") 'senator-previous-tag)
;; (define-key tern-mode-keymap (kbd "M-f") 'senator-fold-tag-toggle)
(define-key tern-mode-keymap (kbd "M-<next>") nil)
(define-key tern-mode-keymap (kbd "M-<prior>") nil)
(define-key tern-mode-keymap (kbd "M-f") nil)
(define-key tern-mode-keymap (kbd "<tab>") 'company-indent-or-complete-common)

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
