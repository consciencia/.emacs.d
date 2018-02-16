;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING: following functions are monkey patched EMACS insternals         ;;;
;;; it is strongly advised to check following functions when updating        ;;;
;;; EMACS.                                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun safe-local-variable-p (sym val)
  "Ugly hack to prevent dir-locals floading user with 
qustions..."
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
