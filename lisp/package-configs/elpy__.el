(custom/install-package-when-needed 'elpy)
(require 'elpy)

(setq elpy-modules (delq 'elpy-module-company elpy-modules)
      elpy-rpc-backend "jedi")

(advice-add #'elpy-goto-location
            :after (lambda (&rest args)
                     (pulse-momentary-highlight-one-line (point))
                     (save-mark-and-excursion
                      (semantic-fetch-tags))
                     (recenter)))

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(elpy-company-backend))
            (save-mark-and-excursion
             (semantic-fetch-tags))))

(elpy-enable)



(defun custom/python/regex-parser (regex)
  (let ((cur 1)
        (temp nil)
        (res nil))
    (dolist (line (s-lines (buffer-substring-no-properties
                            (point-min)
                            (point-max))))
      (setq temp (s-match
                  (pcre-to-elisp/cached regex)
                  line))
      (if temp
          (setq res (cons (list (cadr temp) cur) res)))
      (setq cur (+ cur 1)))
    res))

(defun custom/python/parse-tags ()
  (interactive)
  (append (custom/python/regex-parser "^\\s*def\\s+(\\w+)")
          (custom/python/regex-parser "^\\s*class\\s+(\\w+)")
          (custom/python/regex-parser "^\\s*import\\s+([\\w\\.]+)")))


(defun custom/python/go-to-tag ()
  (interactive)
  (let ((tags (custom/python/parse-tags))
        (res nil))
    (setq res (ido-completing-read "Symbol?"
                                   (mapcar #'car tags)))
    (xref-push-marker-stack)
    (goto-line (cadar (seq-filter (lambda (x) (equal res (car x)))
                                  tags)))
    (pulse-momentary-highlight-one-line (point))))



;; elpy-mode-map
(define-key elpy-mode-map (kbd "<C-down>")
  'elpy-nav-forward-block)
(define-key elpy-mode-map (kbd "<C-up>")
  'elpy-nav-backward-block)
(define-key elpy-mode-map (kbd "<C-left>")
  'elpy-nav-backward-indent)
(define-key elpy-mode-map (kbd "<C-right>")
  'elpy-nav-forward-indent)
(define-key elpy-mode-map (kbd "<tab>")
  'company-indent-or-complete-common)
(define-key elpy-mode-map (kbd "<M-down>") nil)
(define-key elpy-mode-map (kbd "<M-up>") nil)
(define-key elpy-mode-map (kbd "<M-left>") nil)
(define-key elpy-mode-map (kbd "<M-right>") nil)
(define-key elpy-mode-map (kbd "M-<next>")
  ;; 'custom/python/next-defun
  nil)
(define-key elpy-mode-map (kbd "M-<prior>")
  ;; 'custom/python/prev-defun
  nil)
(define-key elpy-mode-map (kbd "M-f") nil)
(define-key elpy-mode-map (kbd "M-*") 'elpy-doc)
(define-key elpy-mode-map (kbd "M-d")
        (lambda ()
          (interactive)
          (call-interactively 'python-mark-defun)
          (setq transient-mark-mode (cons 'only transient-mark-mode))))
(define-key elpy-mode-map (kbd "C-<right>") 'custom/forward-symbol)
(define-key elpy-mode-map (kbd "C-<left>") 'custom/backward-symbol)
(define-key elpy-mode-map (kbd "C-<down>") 'forward-paragraph)
(define-key elpy-mode-map (kbd "C-<up>") 'backward-paragraph)
(define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition)
(define-key elpy-mode-map (kbd "M-,") (lambda ()
                                        (interactive)
                                        (xref-pop-marker-stack)
                                        (pulse-momentary-highlight-one-line (point))
                                        (recenter)))
