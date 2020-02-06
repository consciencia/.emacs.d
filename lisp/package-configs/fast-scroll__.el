(require 'fast-scroll)


(fast-scroll-mode 1)


(setq custom/scrolling-flag nil)

(defun custom/started-scrolling ()
  (setq custom/scrolling-flag t))

(defun custom/stopped-scrolling ()
  (setq custom/scrolling-flag nil))

(defun custom/around-jit-lock-function (old-fn &rest args)
  (when (not custom/scrolling-flag)
    (apply old-fn args)))


(advice-add #'jit-lock-function :around 'custom/around-jit-lock-function)
(add-hook 'fast-scroll-start-hook 'custom/started-scrolling)
(add-hook 'fast-scroll-end-hook 'custom/stopped-scrolling)
