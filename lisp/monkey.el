;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING: following functions are monkey patched EMACS insternals         ;;;
;;; it is strongly advised to check following functions when updating        ;;;
;;; EMACS.                                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'xref)


;; (defun safe-local-variable-p (sym val)
;;   "Ugly hack to prevent dir-locals floading user with
;; qustions. On the other side, it opens a way to exploits based
;; on local directory elisp code. TODO, solve this somehow!"
;;   t)

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

;; Overriden because I want to be able to execute elisp in regexp
;; input and use its result value as an final input.
(defun occur-read-primary-args ()
  (let* ((perform-collect (consp current-prefix-arg))
         (regexp (read-regexp (if perform-collect
                                  "Collect strings matching regexp"
                                "List lines matching regexp")
                              'regexp-history-last))
         (code (ignore-errors (read regexp))))
    (when (consp code)
      (setq regexp (format "%s" (eval code))))
    (list regexp
          (if perform-collect
              ;; Perform collect operation
              (if (zerop (regexp-opt-depth regexp))
                  ;; No subexpression so collect the entire match.
                  "\\&"
                ;; Get the regexp for collection pattern.
                (let ((default (car occur-collect-regexp-history)))
                  (read-regexp
                   (format "Regexp to collect (default %s): " default)
                   default 'occur-collect-regexp-history)))
            ;; Otherwise normal occur takes numerical prefix argument.
            (when current-prefix-arg
              (prefix-numeric-value current-prefix-arg))))))

;; Overriden because I want to be able to parse number from input
;; with noise.
(defun read-number (prompt &optional default)
  "Read a numeric value in the minibuffer, prompting with PROMPT.
DEFAULT specifies a default value to return if the user just types RET.
The value of DEFAULT is inserted into PROMPT.
This function is used by the `interactive' code letter `n'."
  (let ((n nil)
        (default1 (if (consp default) (car default) default)))
    (when default1
      (setq prompt
            (if (string-match "\\(\\):[ \t]*\\'" prompt)
                (replace-match (format " (default %s)" default1) t t prompt 1)
              (replace-regexp-in-string "[ \t]*\\'"
                                        (format " (default %s) " default1)
                                        prompt t t))))
    (while
        (progn
          (let ((str (read-from-minibuffer
                      prompt nil nil nil nil
                      (when default
                        (if (consp default)
                            (mapcar 'number-to-string (delq nil default))
                          (number-to-string default))))))
            (condition-case nil
                (setq n (cond
                         ((zerop (length str)) default1)
                         ((stringp str)
                          (or (let* ((parser (pcre-to-elisp/cached
                                              "[^\\d]*(\\d+(?:\\.\\d+)?)[^\\d]*"))
                                     (capture (s-match parser str)))
                                (when capture
                                  (read (cadr capture))))
                              (read str)))))
              (error nil)))
          (unless (numberp n)
            (message "Please enter a number.")
            (sit-for 1)
            t)))
    n))
