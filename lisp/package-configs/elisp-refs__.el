(custom/install-package-when-needed 'elisp-refs)
(require 'elisp-refs)



(advice-add #'elisp-refs-visit-match
            :before
            (lambda (&rest args)
              (custom/universal-push-mark)))

(advice-add #'elisp-refs--find-file
            :before
            (lambda (&rest args)
              (custom/universal-push-mark)))

(advice-add #'elisp-refs-symbol
            :before
            (lambda (&rest args)
              (custom/universal-push-mark)))



;; Reworked because old one lacked initial input.
(defun elisp-refs--completing-read-symbol (prompt &optional filter)
  "Read an interned symbol from the minibuffer,
defaulting to the symbol at point. PROMPT is the string to prompt
with.

If FILTER is given, only offer symbols where (FILTER sym) returns
t."
  (let ((filter (or filter (lambda (_) t))))
    (read
     (completing-read prompt
                      (elisp-refs--filter-obarray filter)
                      nil nil (thing-at-point 'symbol) nil
                      (-if-let (sym (thing-at-point 'symbol))
                          (when (funcall filter (read sym))
                            sym))))))

(defun custom/elip-refs-quit ()
  (interactive)
  (kill-this-buffer)
  (custom/universal-pop-mark))



(define-key elisp-refs-mode-map (kbd "q") 'custom/elip-refs-quit)
(define-key elisp-refs-mode-map (kbd "M-,") 'custom/elip-refs-quit)
