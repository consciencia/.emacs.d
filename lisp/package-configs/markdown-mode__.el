(custom/install-package-when-needed 'markdown-mode)
(require 'markdown-mode)



(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))



(add-hook 'markdown-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (flyspell-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link auto hide
;;
;; TODO:
;; turn overlayed text to be read only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun custom/find-all-link-ranges ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (pcre-to-elisp/cached "https?:\\/\\/[^\\s\\]]+"))
          (matches nil)
          (match nil))
      (while (search-forward-regexp regexp nil t)
        (setq match (cons (match-beginning 0)
                          (match-end 0)))
        (when (save-excursion
                (goto-char (1- (cdr match)))
                (looking-at (pcre-to-elisp/cached "\\.|\\)")))
          (setf (cdr match)
                (1- (cdr match))))
        (push match matches))
      (when (interactive-p)
        (let ((links nil))
          (setq links
                (loop for (start . end) in matches
                      collect (buffer-substring-no-properties start
                                                              end)))
          (custom/with-simple-pop-up "*links*"
            (loop for link in links
                  do (insert link "\n")))))
      matches)))

(defun custom/get-link-domain (start end)
  (save-excursion
    (goto-char start)
    (if (search-forward-regexp (pcre-to-elisp/cached
                                "https?:\\/\\/([^\\/]+)")
                               end t)
        (let ((result (match-string-no-properties 1)))
          (if result
              result
            "error"))
      "error")))

(defun custom/hide-all-links ()
  (interactive)
  (loop for (link-start . link-end) in (custom/find-all-link-ranges)
        if (let ((already-hidden nil))
             (dolist (ov (overlays-in link-start link-end))
               (when (overlay-get ov 'hidden-link-marker)
                 (setq already-hidden t)))
             (not already-hidden))
        if (not (custom/val-in-range (point)
                                     (cons link-start
                                           link-end)))
        do (let ((ov (make-overlay link-start
                                   link-end
                                   (current-buffer))))
             (overlay-put ov 'hidden-link-marker t)
             (overlay-put ov 'evaporate t)
             (overlay-put ov 'display
                          (concat "<link: "
                                  (custom/get-link-domain link-start
                                                          link-end)
                                  ">"))
             (overlay-put ov 'face 'link))))

(defun custom/show-all-links ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'hidden-link-marker)
        (delete-overlay ov)))))

(defun custom/link-overlay-at-point (&optional pos)
  (interactive)
  (setq pos (or pos (point)))
  (let ((overlay nil)
        (content nil))
    (dolist (ov (overlays-at pos))
      (when (and (overlay-get ov 'hidden-link-marker)
                 (not overlay))
        (setq overlay ov)
        (setq content (buffer-substring-no-properties
                       (overlay-start ov)
                       (overlay-end ov)))))
    (when (not (and (not overlay)
                    (not content)))
      (cons overlay content))))

(defun custom/show-link-on-hover ()
  (when (equal major-mode 'markdown-mode)
    (let ((link (custom/link-overlay-at-point)))
      (when link
        (delete-overlay (car link))))))

(defun custom/idle-hide-all-links ()
  (when (and (equal major-mode 'markdown-mode)
             (not (region-active-p)))
    (custom/hide-all-links)))

;; (cancel-function-timers 'custom/show-link-on-hover)
(run-with-idle-timer 0.1 t 'custom/show-link-on-hover)
;; (cancel-function-timers 'custom/idle-hide-all-links)
(run-with-idle-timer 0.1 t 'custom/idle-hide-all-links)
