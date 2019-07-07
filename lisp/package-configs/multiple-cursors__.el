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


(setq *cutom/bulk-clipboard* nil)

(mc/load-lists)
(push 'custom/copy-across-cursors
      mc/cmds-to-run-once)
(push 'cua-copy-region
      mc/cmds-to-run-once)


(defun custom/copy-across-cursors ()
  (interactive)
  (mc/save-excursion
   (mc/save-window-scroll
    (mc/for-each-fake-cursor
     (save-excursion
       (custom/copy-across-cursor cursor)))))
  (mc--reset-read-prompts)
  (push (buffer-substring
         (caar (region-bounds))
         (cdar (region-bounds)))
        *cutom/bulk-clipboard*)
  (kill-new (s-chomp (loop for x in *cutom/bulk-clipboard*
                           concat (concat x "\n"))))
  (setq *cutom/bulk-clipboard* nil))

(defun custom/copy-across-cursor (cursor)
  (let ((mc--executing-command-for-fake-cursor t)
        (id (overlay-get cursor 'mc-id))
        (annoying-arrows-mode nil)
        (smooth-scroll-margin 0))
    (mc/add-fake-cursor-to-undo-list
     (mc/pop-state-from-overlay cursor)
     (ignore-errors
       (custom/cursor-handle-copy)
       (mc/create-fake-cursor-at-point id)))))

(defun custom/cursor-handle-copy ()
  (when (region-active-p)
    (push (buffer-substring
           (caar (region-bounds))
           (cdar (region-bounds)))
          *cutom/bulk-clipboard*))
  (when deactivate-mark (deactivate-mark)))


(advice-add #'cua-copy-region
            :around (lambda (oldfn &rest args)
                      (if (> (mc/num-cursors) 1)
                          (custom/copy-across-cursors)
                        (apply oldfn args))))


(define-key mc/keymap (kbd "<ESC>") 'mc/keyboard-quit)
