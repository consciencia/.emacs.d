(custom/install-package-when-needed 'fast-scroll)
(require 'fast-scroll)



(defun fast-scroll-advice-scroll-functions ()
  "Wrap as many scrolling functions that we know of in this advice."
  (interactive)
  (advice-add #'scroll-up :around #'fast-scroll-run-fn-minimally)
  (advice-add #'scroll-down :around #'fast-scroll-run-fn-minimally)
  (advice-add #'scroll-up-command :around #'fast-scroll-run-fn-minimally)
  (advice-add #'scroll-down-command :around #'fast-scroll-run-fn-minimally)
  (advice-add #'evil-scroll-up :around #'fast-scroll-run-fn-minimally)
  (advice-add #'evil-scroll-down :around #'fast-scroll-run-fn-minimally))

(defun fast-scroll-unload-function ()
  "Remove advice added by `fast-scroll-advice-scroll-functions'.
Note this function's name implies compatibility with `unload-feature'."
  (interactive)
  (advice-remove #'scroll-up #'fast-scroll-run-fn-minimally)
  (advice-remove #'scroll-down #'fast-scroll-run-fn-minimally)
  (advice-remove #'scroll-up-command #'fast-scroll-run-fn-minimally)
  (advice-remove #'scroll-down-command #'fast-scroll-run-fn-minimally)
  (advice-remove #'evil-scroll-up #'fast-scroll-run-fn-minimally)
  (advice-remove #'evil-scroll-down #'fast-scroll-run-fn-minimally)
  nil)



(fast-scroll-mode 1)

;; TODO:
;; Get rid of calls to jit-lock-function using
;;     (add-hook 'fast-scroll-start-hook (lambda () ))
;;     (add-hook 'fast-scroll-end-hook (lambda () ))
