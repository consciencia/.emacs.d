(custom/install-package-when-needed 'elpy)
(require 'elpy)


(setq elpy-modules
      (delq 'elpy-module-company
            elpy-modules))
(setq elpy-rpc-backend "jedi")

(advice-add #'elpy-goto-location
            :after (lambda (&rest args)
                     (pulse-momentary-highlight-one-line (point))
                     (recenter)))

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(elpy-company-backend))
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))
            (when (not (eq system-type 'windows-nt))
              (flyspell-prog-mode))))

(elpy-enable)


(defun custom/python/indent-or-complete (arg)
  (interactive "P")
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((memq indent-line-function
          '(indent-relative indent-relative-maybe))
    (company-complete-common))
   ((let ((tab-always-indent t)
          (candidates nil))
      (ignore-errors
        (when (company-manual-begin)
          (setq candidates company-candidates)
          (company-abort)))
      (if (and (save-excursion
                 (backward-char)
                 (looking-at-p
                  (pcre-to-elisp/cached
                   "\\s+")))
               (save-excursion
                 (backward-char 3)
                 (not (looking-at-p
                       (pcre-to-elisp/cached
                        "(?:if|in|as)\\s"))))
               (save-excursion
                 (backward-char 5)
                 (not (looking-at-p
                       (pcre-to-elisp/cached
                        "with\\s")))))
          (indent-for-tab-command arg)
        (if (> (length candidates) 0)
            (company-complete-common)
          (indent-for-tab-command arg)))))))


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
  'custom/python/indent-or-complete)
(define-key elpy-mode-map (kbd "<M-down>") nil)
(define-key elpy-mode-map (kbd "<M-up>") nil)
(define-key elpy-mode-map (kbd "<M-left>") nil)
(define-key elpy-mode-map (kbd "<M-right>") nil)
(define-key elpy-mode-map (kbd "M-<next>")
  'python-nav-forward-defun)
(define-key elpy-mode-map (kbd "M-<prior>")
  'python-nav-backward-defun)
(define-key elpy-mode-map (kbd "M-f") nil)
(define-key elpy-mode-map (kbd "M-*") 'elpy-doc)
(define-key elpy-mode-map (kbd "M-d") 'custom/mark-defun)
(define-key elpy-mode-map (kbd "C-<right>") 'custom/forward-symbol)
(define-key elpy-mode-map (kbd "C-<left>") 'custom/backward-symbol)
(define-key elpy-mode-map (kbd "C-<down>") 'forward-paragraph)
(define-key elpy-mode-map (kbd "C-<up>") 'backward-paragraph)
(define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition)
(define-key elpy-mode-map (kbd "M-,") (lambda ()
                                        (interactive)
                                        (xref-pop-marker-stack)
                                        (pulse-momentary-highlight-one-line
                                         (point))
                                        (recenter)))
