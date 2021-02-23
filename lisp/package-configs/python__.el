(custom/install-package-when-needed 'company-jedi)

(require 'python)
(require 'jedi-core)

(advice-add #'jedi:find-file
            :before (lambda (&rest args)
                      (custom/universal-push-mark)))

(advice-add #'jedi:goto--line-column
            :after (lambda (&rest args)
                     (pulse-momentary-highlight-one-line (point))
                     (recenter)))

(advice-add #'jedi:goto-definition--nth
            :around
            (lambda (oldfn other-window &optional try-next)
              (let* ((entries (loop for entry in jedi:goto-definition--cache
                                    for min-entry = (cl-destructuring-bind
                                                        (&key line_nr
                                                              column
                                                              module_path
                                                              module_name
                                                              &allow-other-keys)
                                                        entry
                                                      (list module_name
                                                            module_path
                                                            line_nr
                                                            column))
                                    if (not (string= (car min-entry) "__builtin__"))
                                    if (file-exists-p (cadr min-entry))
                                    collect min-entry))
                     (summarizer (lambda (entry)
                                   (concat (nth 1 entry)
                                           ":"
                                           (format "%s" (nth 2 entry)))))
                     (chosen-entry (custom/ido-completing-read-ctx entries summarizer)))
                (setq jedi:goto-definition--cache nil)
                (when (null chosen-entry)
                  (error "Failed to find definition (%s %s)!"
                         (length jedi:goto-definition--cache)
                         (length entries)))
                (jedi:find-file (nth 1 chosen-entry)
                                (nth 2 chosen-entry)
                                (nth 3 chosen-entry)
                                other-window)
                (run-hooks 'jedi:goto-definition-hook))))

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-jedi company-files))
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))
            (when (not (eq system-type 'windows-nt))
              (flyspell-prog-mode))
            (jedi:setup)
            (flycheck-mode)))


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

(defun custom/py-normalize-region ()
  "If the first or last line are not fully
selected, select them completely."
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char beg)
    (beginning-of-line)
    (push-mark (point) nil t)
    (goto-char end)
    (unless (= (point) (line-beginning-position))
      (end-of-line))))

(defun custom/py-indent-shift-right (&optional count)
  "Shift current line by COUNT columns to the right.

COUNT defaults to `python-indent-offset'.
If region is active, normalize the region and shift."
  (interactive)
  (if (use-region-p)
      (progn
        (custom/py-normalize-region)
        (python-indent-shift-right (region-beginning)
                                   (region-end)
                                   current-prefix-arg))
    (python-indent-shift-right (line-beginning-position)
                               (line-end-position)
                               current-prefix-arg)))

(defun custom/py-indent-shift-left (&optional count)
  "Shift current line by COUNT columns to the left.

COUNT defaults to `python-indent-offset'.
If region is active, normalize the region and shift."
  (interactive)
  (if (use-region-p)
      (progn
        (custom/py-normalize-region)
        (python-indent-shift-left (region-beginning)
                                  (region-end)
                                  current-prefix-arg))
    (python-indent-shift-left (line-beginning-position)
                              (line-end-position)
                              current-prefix-arg)))


(define-key jedi-mode-map (kbd "M-.") 'jedi:goto-definition)
(define-key jedi-mode-map (kbd "M-,") 'custom/universal-pop-mark)
(define-key jedi-mode-map (kbd "M--") nil)
(define-key jedi-mode-map (kbd "M-*") 'jedi:show-doc)
(define-key jedi-mode-map (kbd "<tab>") 'custom/python/indent-or-complete)
(define-key jedi-mode-map (kbd "M-<next>") 'python-nav-forward-defun)
(define-key jedi-mode-map (kbd "M-<prior>") 'python-nav-backward-defun)
(define-key jedi-mode-map (kbd "<C-kp-add>") 'custom/py-indent-shift-right)
(define-key jedi-mode-map (kbd "<C-kp-subtract>") 'custom/py-indent-shift-left)
(define-key jedi-mode-map (kbd "M-d") 'custom/mark-defun)
(define-key jedi-mode-map (kbd "M-a") 'custom/mark-args)
