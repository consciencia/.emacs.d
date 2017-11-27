(setq custom/separators-regexp
      (concat "\\([\-'\"(){};:,.\\/?!@#%&*+=\]\\)\\|\\(\\[\\)\\|"
              "\\(\\]\\)\\|\\(\\s-\\)\\|\\(\\s_\\)\\|\\(\\`\\)\\|"
              "\\(\\'\\)\\|\\(\\^\\)\\|\\(\n\\)"))

(defun custom/position-normalize-forward (saved-pos my-pos)
  "Normalize position after forward move"
  (if (>= (- my-pos saved-pos) 2)
      (progn
                                        ;going to my-pos
        (goto-char my-pos)
        (if (not (eq my-pos (point-max)))
            (progn
                                        ;also backward-to-separator to fix position
              (custom/backward-symbol))))
    (progn
                                        ;going to my-pos in the usual way
      (goto-char my-pos))))

(defun custom/forward-symbol ()
  "Move to the next separator like in the every NORMAL editor"
  (interactive "^")
  (let ((saved-pos (point))
        (my-pos (re-search-forward custom/separators-regexp)))
    (if (eq my-pos 1)
        (progn
          (goto-char (+ my-pos 1))
          (setf my-pos (re-search-forward custom/separators-regexp))
          (custom/position-normalize-forward saved-pos my-pos))
      (custom/position-normalize-forward saved-pos my-pos))))


(defun custom/position-normalize-backward (saved-pos my-pos)
  "Normalize position after backward move"
  (if (>= (- saved-pos my-pos) 2)
      (progn
        (goto-char my-pos)
        (if (not (eq my-pos 1))
            (progn
              (custom/forward-symbol))))
    (progn
      (goto-char my-pos))))

;;See how it works at custom/position-normalize-forward
(defun custom/backward-symbol ()
  "Move to the previous separator like in the every NORMAL editor"
  (interactive "^")
  (let ((saved-pos (point))
        (my-pos (re-search-backward custom/separators-regexp)))
    (if (eq my-pos (point-max))
        (progn
          (goto-char (- my-pos 1))
          (setf my-pos (re-search-backward custom/separators-regexp))
          (custom/position-normalize-backward saved-pos my-pos))
      (custom/position-normalize-backward saved-pos my-pos))))


(defun custom/position-normalize-backspace (saved-pos my-pos)
  (if (>= (- saved-pos my-pos) 2)
      (progn
        (goto-char my-pos)
        (if (not (eq my-pos 1))
            (progn
              (custom/forward-symbol)
              (let ((new-pos (point)))
                (delete-region new-pos saved-pos)))
          (progn
            (delete-region 1 saved-pos))))
    (progn
      (goto-char my-pos)
      (let ((new-pos (point)))
        (delete-region new-pos saved-pos)))))

(defun custom/backward-symbol-erase ()
  "Erase characters to the first separator"
  (interactive)
  (let ((saved-pos (point))
        (my-pos (re-search-backward custom/separators-regexp)))
    (if (eq my-pos (point-max))
        (progn
          (goto-char (- my-pos 1))
          (setf my-pos (re-search-backward custom/separators-regexp))
          (custom/position-normalize-backspace saved-pos my-pos))
      (custom/position-normalize-backspace saved-pos my-pos))))
