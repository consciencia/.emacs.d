(load "too-long-lines-mode.el")

(too-long-lines-mode)

(setq too-long-lines-threshold 320
      too-long-lines-show-number-of-characters 50)



;; Added check for hidden-link-marker in order to not hide too long
;; lines which contains hidden links.
(defun too-long-lines-hide (&optional beg end len)
  "Hides lines that are longer then `too-long-lines-threshold'.

It replaces too long lines with the first N number characters of the line as
configured in variable `too-long-lines-show-number-of-characters', and a little
info blurp about how many characters were hidden.

See also `too-long-lines-threshold', `too-long-lines-show-number-of-characters',
`too-long-lines-hide-in-buffers' and `too-long-lines-show'."
  (interactive)
  (save-excursion
    (goto-char (or beg (point-min)))
    (let ((done nil))
      (while (not done)
        (setq done (>= (line-end-position) (or end (point-max))))
        (let ((line-length (- (line-end-position) (line-beginning-position)))
              (already-hidden nil))
          (when (> line-length too-long-lines-threshold)
            (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
              (if (overlay-get ov 'hidden-link-marker)
                  (setq already-hidden t)
                (if (and (overlay-get ov 'too-long-line)
                         (> (line-end-position) (overlay-end ov)))
                    (delete-overlay ov)
                  (setq already-hidden t)
                  )))
            (unless already-hidden
              (let ((ov (make-overlay (+ (line-beginning-position) too-long-lines-show-number-of-characters) (line-end-position) (current-buffer))))
                (overlay-put ov 'too-long-line t)
                (overlay-put ov 'display (concat "... " (prin1-to-string (- line-length too-long-lines-show-number-of-characters)) " hidden characters"))
                (overlay-put ov 'face '(:background "#ff0066" :foreground "white"))
                ))))
        (when (eq (point) (goto-char (line-beginning-position 2)))
          (setq done t))))))
