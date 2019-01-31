;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING: following functions are monkey patched EMACS insternals         ;;;
;;; it is strongly advised to check following functions when updating        ;;;
;;; EMACS.                                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(advice-add #'file-exists-p
            :around (lambda (oldfn filename)
                      (if filename
                          (apply oldfn (list filename))
                        nil)))

(defun safe-local-variable-p (sym val)
  "Ugly hack to prevent dir-locals floading user with
qustions. On the other side, it opens a way to exploits based
on local directory elisp code. TODO, solve this somehow!"
  t)

(defun keyboard-escape-quit ()
  "Modifies EMACs keyboard-escape-quit but withut window removal."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
        ((region-active-p)
         (deactivate-mark))
        ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (current-prefix-arg
         nil)
        ((> (recursion-depth) 0)
         (exit-recursive-edit))
        (buffer-quit-function
         (funcall buffer-quit-function))
        ((string-match "^ \\*" (buffer-name (current-buffer)))
         (bury-buffer))))

;; TODO
;; (defun aggressive-indent-indent-region-and-on (l r)
;;   "Indent region between L and R, and then some.
;; Call `aggressive-indent-region-function' between L and R, and
;; then keep indenting until nothing more happens."
;;   (interactive "r")
;;   (let ((p (point-marker))
;;         was-begining-of-line)
;;     (set-marker-insertion-type p t)
;;     (unwind-protect
;;         (progn
;;           (unless (= l r)
;;             (when (= (char-before r) ?\n)
;;               (cl-decf r)))
;;           ;; If L is at the end of a line, skip that line.
;;           (unless (= l r)
;;             (when (= (char-after l) ?\n)
;;               (cl-incf l)))
;;           ;; Indent the affected region.
;;           (goto-char r)
;;           (unless (= l r) (funcall aggressive-indent-region-function l r))
;;           ;; And then we indent each following line until nothing happens.
;;           (forward-line 1)
;;           (skip-chars-forward "[:blank:]\n\r\xc")
;;           ;; try to indent via indent line, when point changes
;;           ;; repeat otherwise just stop
;;           ;; following code is considered good for nothing so remove
;;           ;; it completely
;;           (let ((base-column (current-column)))
;;             (while (and (not (eobp))
;;                         (not (run-hook-with-args-until-success 'aggressive-indent-stop-here-hook))
;;                         (aggressive-indent--indent-current-balanced-line base-column)))))
;;       (goto-char p))))
