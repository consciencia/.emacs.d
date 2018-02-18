(require 'cl-macs)

(setq custom/separators-regexp
      "\\([]['\"(){};:,.\\/?!@#%&*+=^[:space:]\n-]\\|\\s_\\|\\`\\|‌​‌​\\'\\)")

(defun custom/position-normalize-forward (saved-pos my-pos)
  (if (>= (- my-pos saved-pos) 2)
      (progn
        (goto-char my-pos)
        (if (not (eq my-pos (point-max)))
            (progn
              (custom/backward-symbol))))
    (progn
      (goto-char my-pos))))

(defun custom/forward-symbol ()
  (interactive "^")
  (block custom/forward-symbol
    (let* ((saved-pos (point))
           (my-pos (re-search-forward custom/separators-regexp))
           (my-pos2 (custom/find-blank-space t saved-pos)))
      (if (> my-pos2 my-pos)
          (progn
            (goto-char (- my-pos2 1))
            (return-from custom/forward-symbol)))
      (if (eq my-pos 1)
          (progn
            (goto-char (+ my-pos 1))
            (setf my-pos (re-search-forward custom/separators-regexp))
            (custom/position-normalize-forward saved-pos my-pos))
        (custom/position-normalize-forward saved-pos my-pos)))))


(defun custom/position-normalize-backward (saved-pos my-pos)
  (if (>= (- saved-pos my-pos) 2)
      (progn
        (goto-char my-pos)
        (if (not (eq my-pos 1))
            (progn
              (custom/forward-symbol))))
    (progn
      (goto-char my-pos))))

(defun custom/backward-symbol ()
  (interactive "^")
  (block custom/backward-symbol
    (let* ((saved-pos (point))
           (my-pos (re-search-backward custom/separators-regexp))
           (my-pos2 (custom/find-blank-space nil saved-pos)))
      (if (< my-pos2 my-pos)
          (progn
            (goto-char (+ my-pos2 1))
            (return-from custom/backward-symbol)))
      (if (eq my-pos (point-max))
          (progn
            (goto-char (- my-pos 1))
            (setf my-pos (re-search-backward custom/separators-regexp))
            (custom/position-normalize-backward saved-pos my-pos))
        (custom/position-normalize-backward saved-pos my-pos)))))

(defun custom/find-blank-space (go-forward starting-pos)
  (let ((starting-char (string (char-after starting-pos)))
        (skipped-count 0)
        (increment (if go-forward 1 -1))
        (should-loop t))
    (if (or (equal starting-char " ")
            (equal starting-char "\t")
            (equal starting-char "\n"))
        (progn
          (while should-loop
            (if (or (equal (string (char-after (+ saved-pos skipped-count))) " ")
                    (equal (string (char-after (+ saved-pos skipped-count))) "\t")
                    (equal (string (char-after (+ saved-pos skipped-count))) "\n"))
                (setq skipped-count (+ skipped-count increment))
              (setq should-loop nil)))
          (+ starting-pos skipped-count))
      starting-pos)))
