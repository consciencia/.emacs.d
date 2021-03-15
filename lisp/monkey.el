;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING: following functions are monkey patched EMACS insternals         ;;;
;;; it is strongly advised to check following functions when updating        ;;;
;;; EMACS.                                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'xref)


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

(advice-add #'elisp-last-sexp-toggle-display
            :around
            (lambda (oldfn &rest args)
              (if (thing-at-point 'symbol)
                  (apply oldfn args)
                (insert "\n"))))

(advice-add #'occur-mode-goto-occurrence
            :after
            (lambda (&rest args)
              (recenter)))

;; Overriden because old implementation had horrible policy when
;; jumping around. Instead of opening new buffer in window with xref
;; results, it opened new buffer in window from which xref was
;; originally invoked.
(defun xref--show-pos-in-buf (pos buf)
  "Goto and display position POS of buffer BUF in a window.
Honor `xref--original-window-intent', run `xref-after-jump-hook'
and finally return the window."
  (let ((xref-buf (current-buffer)))
    (custom/universal-push-mark)
    (switch-to-buffer buf)
    (xref--goto-char pos)
    (run-hooks 'xref-after-jump-hook)
    (with-current-buffer xref-buf
      (setq-local other-window-scroll-buffer
                  (current-buffer)))
    (selected-window)))
