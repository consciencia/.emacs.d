(custom/install-package-when-needed 'multiple-cursors)
(require 'multiple-cursors)

(setq mc/always-repeat-command t
      mc/edit-lines-empty-lines 'ignore)

(defun custom/mc/mark-next-like-this (arg)
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-after-point)))
        (if cursor
            (mc/remove-fake-cursor cursor)
          (error "No cursors to be unmarked")))
    (mc/mark-lines arg 'forwards))
  (mc/maybe-multiple-cursors-mode))

(defun custom/mc/mark-prev-like-this (arg)
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




