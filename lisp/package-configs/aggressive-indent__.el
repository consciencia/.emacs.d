(custom/install-package-when-needed 'aggressive-indent)
(require 'aggressive-indent)



(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'html-mode-hook #'aggressive-indent-mode)
(add-hook 'js-mode-hook #'aggressive-indent-mode)
(add-hook 'c-mode-hook #'aggressive-indent-mode)
(add-hook 'c++-mode-hook #'aggressive-indent-mode)



(setq *custom/agressive-indent-disabled* nil)
(push '*custom/agressive-indent-disabled*
      aggressive-indent-dont-indent-if)

(defun custom/toggle-agressive-indent ()
  (interactive)
  (setq *custom/agressive-indent-disabled*
        (not *custom/agressive-indent-disabled*))
  (if *custom/agressive-indent-disabled*
      (message "Agressive indent is disabled")
    (message "Agressive indent is enabled")))

(defun custom/disable-agressive-indent ()
  (interactive)
  (setq *custom/agressive-indent-disabled* t)
  (message "Agressive indent is disabled"))

(defun custom/enable-agressive-indent ()
  (interactive)
  (setq *custom/agressive-indent-disabled* nil)
  (message "Agressive indent is enabled"))

(defun custom/agressive-indent-state ()
  (not *custom/agressive-indent-disabled*))

(defun aggressive-indent-indent-region-and-on (l r)
  "Indent region between L and R, and then some.
Call `aggressive-indent-region-function' between L and R, and
then keep indenting until nothing more happens."
  (interactive "r")
  (let ((p (point-marker))
        was-begining-of-line)
    (set-marker-insertion-type p t)
    (unwind-protect
        (progn
          (unless (= l r)
            (when (= (char-before r) ?\n)
              (cl-decf r)))
          ;; If L is at the end of a line, skip that line.
          (unless (= l r)
            (when (= (char-after l) ?\n)
              (cl-incf l)))
          ;; Indent the affected region.
          (goto-char r)
          (unless (= l r) (funcall aggressive-indent-region-function l r))
          ;; And then we indent each following line until nothing happens.
          ;; BAD CODE
          ;; (forward-line 1)
          ;; (skip-chars-forward "[:blank:]\n\r\xc")
          ;; (let ((base-column (current-column)))
          ;;   (while (and (not (eobp))
          ;;               (not (run-hook-with-args-until-success 'aggressive-indent-stop-here-hook))
          ;;               (aggressive-indent--indent-current-balanced-line base-column))))
          ;; BAD CODE
          ;; BEGIN OF FIX
          (let ((run t)
                (oldpos nil)
                left right)
            (while run
              (forward-line 1)
              (setq left (point))
              (end-of-visual-line)
              (setq right (point))
              (goto-char left)
              (skip-chars-forward "[:blank:]\n\r\xc")
              (setq oldpos (point))
              (funcall aggressive-indent-region-function left right)
              (if (equal oldpos (point))
                  (setq run nil))))
          ;; END OF FIX
          )
      (goto-char p))))
