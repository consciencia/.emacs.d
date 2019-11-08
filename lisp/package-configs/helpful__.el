(custom/install-package-when-needed 'helpful)
(require 'helpful)



(advice-add #'helpful--all-references
            :before
            (lambda (&rest args)
              (custom/universal-push-mark)))



;; Reworked because original impl was missing faces and packages.
(defun helpful-symbol (symbol)
  "Show help for SYMBOL, a variable, function or macro.

See also `helpful-callable' and `helpful-variable'."
  (interactive
   (list (helpful--read-symbol "Symbol: " #'helpful--bound-p)))
  (let ((c-var-sym (helpful--convert-c-name symbol t))
        (c-fn-sym (helpful--convert-c-name symbol nil))
        (c-?-sym (intern (symbol-name symbol))))
    (cond
     ((and (boundp symbol) (fboundp symbol))
      (if (y-or-n-p
           (format "%s is a both a variable and a callable, show variable?"
                   symbol))
          (helpful-variable symbol)
        (helpful-callable symbol)))
     ((fboundp symbol)
      (helpful-callable symbol))
     ((boundp symbol)
      (helpful-variable symbol))
     ((and c-fn-sym (fboundp c-fn-sym))
      (helpful-callable c-fn-sym))
     ((and c-var-sym (boundp c-var-sym))
      (helpful-variable c-var-sym))
     ((or (featurep c-?-sym)
          (locate-library (symbol-name symbol)))
      (describe-package symbol)
      (switch-to-buffer-other-window "*Help*"))
     ((facep c-?-sym)
      (describe-face c-?-sym)
      (switch-to-buffer-other-window "*Help*"))
     (t
      (user-error "Not bound: %S" symbol)))))
