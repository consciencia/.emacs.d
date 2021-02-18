;; A LOT faster solution than here:
;;     https://github.com/ahungry/fast-scroll
;; TODO: Report it and try to get it to master
(require 'hi-lock)

(setq custom/scrolling-flag nil)


(defun custom/started-scrolling (&rest args)
  (setq custom/scrolling-flag t))

(defun custom/stopped-scrolling (&rest args)
  (setq custom/scrolling-flag nil))

(defun custom/around-jit-lock-function (old-fn &rest args)
  (when (or (not custom/scrolling-flag)
            (equal major-mode 'dired-mode)
            (equal major-mode 'magit-status-mode)
            (equal major-mode 'magit-log-mode)
            (equal major-mode 'magit-revision-mode)
            (not (null hi-lock-interactive-patterns)))
    (apply old-fn args)))


(advice-add #'previous-line :before #'custom/started-scrolling)
(advice-add #'next-line :before #'custom/started-scrolling)
(advice-add #'scroll-up :before #'custom/started-scrolling)
(advice-add #'scroll-down :before #'custom/started-scrolling)
(advice-add #'scroll-up-command :before #'custom/started-scrolling)
(advice-add #'scroll-down-command :before #'custom/started-scrolling)
(advice-add #'evil-scroll-up :before #'custom/started-scrolling)
(advice-add #'evil-scroll-down :before #'custom/started-scrolling)
(advice-add #'jit-lock-function :around #'custom/around-jit-lock-function)
(run-with-idle-timer 0.1 t #'custom/stopped-scrolling)
