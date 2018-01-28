(custom/install-package-when-needed 'multiple-cursors)
(require 'multiple-cursors)

(defun custom/mc/mark-next-like-this (arg)
  "Find and mark the next part of the buffer matching the currently active region
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-after-point)))
        (if cursor
            (mc/remove-fake-cursor cursor)
          (error "No cursors to be unmarked")))
    (mc/mark-lines arg 'forwards))
  (mc/maybe-multiple-cursors-mode))

(defun custom/mc/mark-prev-like-this (arg)
  "Find and mark the next part of the buffer matching the currently active region
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-before-point)))
        (if cursor
            (mc/remove-fake-cursor cursor)
          (error "No cursors to be unmarked")))
    (mc/mark-lines arg 'backwards))
  (mc/maybe-multiple-cursors-mode))

(setq mc--default-cmds-to-run-once
      (append mc--default-cmds-to-run-once
              '(custom/mc/mark-next-like-this
                custom/mc/mark-prev-like-this)))




