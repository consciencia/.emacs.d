;; -*- lexical-binding: t; -*-
(require 'dash)


(defun custom/kill-buffer-sentinel (process output)
  "Process sentinel to auto kill associated buffer once PROCESS dies."
  (unless (process-live-p process)
    (let* ((buff (process-buffer process))
           (windows (if (buffer-live-p buff)
                        (get-buffer-window-list buff))))
      (when (buffer-live-p buff)
        (loop for win in windows
              do (delete-window win))
        (kill-buffer buff)))))

(defun custom/kill-on-exit-sentinel ()
  "Replace current process sentinel with a new sentinel composed
of the current one and `custom/kill-buffer-sentinel'."
  (let* ((process (get-buffer-process (current-buffer)))
         (og-sentinel (process-sentinel process))
         (sentinel-list (-remove #'null
                                 (list og-sentinel
                                       #'custom/kill-buffer-sentinel)))
         (combined-sentinel
          (lambda (process line)
            (--each sentinel-list
              (funcall it process line)))))
    (setf (process-sentinel process) combined-sentinel)))

(defvar custom/kill-on-exit-comint-hook-has-run nil
  "Whether or not `custom/kill-on-exit-comint-hook' has run or not.
We need this buffer-local var to prevent the hook from running
   several times, as can happen for example when calling `shell'.")

(defun custom/async-funcall (function &optional buffer args delay)
  "Run FUNCTION with ARGS in the buffer after a short DELAY."
  (run-at-time (or delay 0.2)
               nil
               `(lambda ()
                  (with-current-buffer ,buffer
                    ,(cons function args)))))

(defun custom/kill-on-exit-comint-hook ()
  (unless custom/kill-on-exit-comint-hook-has-run
    (setq-local custom/kill-on-exit-comint-hook-has-run t)
    (custom/async-funcall #'custom/kill-on-exit-sentinel
                          (current-buffer))))

(add-hook 'comint-mode-hook #'custom/kill-on-exit-comint-hook)
