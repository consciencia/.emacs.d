(custom/install-package-when-needed 'elpy)
(custom/install-package-when-needed 'company-jedi)

(require 'elpy)
(require 'jedi-core)


;; (setq elpy-modules
;;       (delq 'elpy-module-company
;;             elpy-modules))

;; (advice-add #'elpy-goto-location
;;             :after (lambda (&rest args)
;;                      (pulse-momentary-highlight-one-line (point))
;;                      (recenter)))

(advice-add #'jedi:find-file
            :before (lambda (&rest args)
                      (custom/universal-push-mark)))

(advice-add #'jedi:goto--line-column
            :after (lambda (&rest args)
                     (pulse-momentary-highlight-one-line (point))
                     (recenter)))

(add-hook 'python-mode-hook
          (lambda ()
            ;; (set (make-local-variable 'company-backends)
            ;;      '(elpy-company-backend))
            (set (make-local-variable 'company-backends)
                 '(company-jedi company-files))
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))
            (when (not (eq system-type 'windows-nt))
              (flyspell-prog-mode))
            ;; (when elpy-mode
            ;;   (elpy-rpc-restart))
            (jedi:setup)))


;; (elpy-enable)


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
      (if (and (looking-back (pcre-to-elisp/cached
                              "\\s+")
                             nil)
               (not (looking-back (pcre-to-elisp/cached
                                   "(?:if|in|as|with)\\s")
                                  nil)))
          (indent-for-tab-command arg)
        (progn
          (ignore-errors
            (when (company-manual-begin)
              (setq candidates company-candidates)
              (company-abort)))
          (if (> (length candidates) 0)
              (company-complete-common)
            (indent-for-tab-command arg))))))))


(define-key jedi-mode-map (kbd "M-.") 'jedi:goto-definition)
(define-key jedi-mode-map (kbd "M-,") 'custom/universal-pop-mark)
(define-key jedi-mode-map (kbd "M--") 'xref-find-references)
(define-key jedi-mode-map (kbd "M-*") 'jedi:show-doc)
(define-key jedi-mode-map (kbd "<tab>") 'custom/python/indent-or-complete)
(define-key jedi-mode-map (kbd "M-<next>") 'python-nav-forward-defun)
(define-key jedi-mode-map (kbd "M-<prior>") 'python-nav-backward-defun)
(define-key jedi-mode-map (kbd "<C-kp-add>") 'elpy-nav-indent-shift-right)
(define-key jedi-mode-map (kbd "<C-kp-subtract>") 'elpy-nav-indent-shift-left)
(define-key jedi-mode-map (kbd "M-d") 'custom/mark-defun)
(define-key jedi-mode-map (kbd "M-a") 'custom/mark-args)

;; (define-key elpy-mode-map (kbd "M-f") nil)
;; (define-key elpy-mode-map (kbd "<M-down>") nil)
;; (define-key elpy-mode-map (kbd "<M-up>") nil)
;; (define-key elpy-mode-map (kbd "<M-left>") nil)
;; (define-key elpy-mode-map (kbd "<M-right>") nil)
;; (define-key elpy-mode-map (kbd "C-<right>") 'custom/forward-symbol)
;; (define-key elpy-mode-map (kbd "C-<left>") 'custom/backward-symbol)
;; (define-key elpy-mode-map (kbd "C-<down>") 'forward-paragraph)
;; (define-key elpy-mode-map (kbd "C-<up>") 'backward-paragraph)
;; (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition)
;; (define-key elpy-mode-map (kbd "M-,") 'custom/universal-pop-mark)
;; (define-key elpy-mode-map (kbd "M--") 'xref-find-references)
;; (define-key elpy-mode-map (kbd "M-*") 'elpy-doc)
;; (define-key elpy-mode-map (kbd "<tab>") 'custom/python/indent-or-complete)
;; (define-key elpy-mode-map (kbd "M-<next>") 'python-nav-forward-defun)
;; (define-key elpy-mode-map (kbd "M-<prior>") 'python-nav-backward-defun)
;; (define-key elpy-mode-map (kbd "M-d") 'custom/mark-defun)
;; (define-key elpy-mode-map (kbd "M-a") 'custom/mark-args)
