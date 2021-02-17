;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING: following functions are monkey patched EMACS insternals         ;;;
;;; it is strongly advised to check following functions when updating        ;;;
;;; EMACS.                                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Overrided because of some invalid usage of this function in CEDET.
;;
;; TODO: Find where it is used in CEDET and do fix there instead of monkey
;;       patching emacs internals.
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
