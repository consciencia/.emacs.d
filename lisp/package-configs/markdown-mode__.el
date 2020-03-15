(custom/install-package-when-needed 'markdown-mode)
(require 'markdown-mode)



(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))



(defun custom/markdown-file-opened ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       'custom/line-has-hidden-link)
  (flyspell-mode)
  (custom/hide-all-links)
  (local-set-key (kbd "M-l") 'custom/find-all-link-ranges))
(add-hook 'markdown-mode-hook 'custom/markdown-file-opened)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link auto hide
;;
;; TODO:
;; 1) turn overlayed text to be read only
;;    there will be need to override <backspace> handler in order for
;;    it to work.
;; 2) develop customized too long line colorer
;;    font-lock-add-keywords
;;    (font-lock-add-keywords nil
;;                            '(("\\<\\(FIXME\\):"
;;                               1
;;                               font-lock-warning-face
;;                               t)))
;;
;;    Find text by calling function, and highlight the matches it finds
;;    using font-lock-keyword-face.
;;
;;    When function is called, it receives one argument, the limit of the
;;    search; it should begin searching at point, and not search beyond the
;;    limit. It should return non-nil if it succeeds, and set the match data
;;    to describe the match that was found. Returning nil indicates
;;    failure of the search.
;;
;;    Fontification will call function repeatedly with the same limit, and with
;;    point where the previous invocation left it, until function fails.
;;    On failure, function need not reset point in any particular way.
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
        (if (save-excursion
              (goto-char (- (cdr match) 2))
              (looking-at (pcre-to-elisp/cached "\\)[\\.,]")))
            (setf (cdr match)
                  (- (cdr match) 2))
          (when (save-excursion
                  (goto-char (1- (cdr match)))
                  (looking-at (pcre-to-elisp/cached "\\.|\\)")))
            (setf (cdr match)
                  (1- (cdr match)))))
        (push match matches))
      (when (interactive-p)
        (let ((links nil)
              (old-buffer (current-buffer)))
          (setq links
                (loop for (start . end) in matches
                      collect (cons (cons (current-buffer) start)
                                    (buffer-substring-no-properties start
                                                                    end))))
          (custom/with-simple-pop-up "*links*"
            (setq kill-on-quit t)
            (insert "Found links in "
                    (buffer-file-name old-buffer)
                    ":\n")
            (loop for link in (nreverse links)
                  for linenum = (with-current-buffer (caar link)
                                  (count-lines (point-min) (cdar link)))
                  for real-link = (cadr (s-match (pcre-to-elisp/cached
                                                  "([^#?]*)")
                                                 (cdr link)))
                  do (let ((start-b nil)
                           (end-b nil))
                       (setq start-b (point))
                       (insert "    " (number-to-string linenum) ": ")
                       (insert real-link)
                       (setq end-b (point))
                       (insert "\n")
                       (make-button start-b end-b
                                    'mouse-face 'custom-button-pressed-face
                                    'face nil
                                    'action 'custom/link-button-handler
                                    'linkpos (car link))
                       ;; (add-face-text-property start-b end-b 'underline)
                       (add-face-text-property start-b end-b 'bold))))))
      matches)))

(defun custom/link-button-handler (&optional button)
  (interactive)
  (let ((linkpos (button-get button 'linkpos)))
    (switch-to-buffer-other-window (car linkpos))
    (goto-char (cdr linkpos))))

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
             (overlay-put ov 'keymap
                          (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "<return>")
                              'custom/show-link-on-hover)
                            map))
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

(defun custom/line-has-hidden-link ()
  (interactive)
  (let ((start nil)
        (end nil)
        (has-link nil))
    (save-excursion
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (setq end (point)))
    (dolist (ov (overlays-in start end))
      (when (overlay-get ov 'hidden-link-marker)
        (setq has-link t)))
    has-link))

(defun custom/show-link-on-hover ()
  (interactive)
  (when (equal major-mode 'markdown-mode)
    (let ((link (custom/link-overlay-at-point)))
      (when link
        (delete-overlay (car link))))))

(defun custom/idle-hide-all-links ()
  (when (equal major-mode 'markdown-mode)
    ;; whitespace mode uses font lock regexes to fontify too long
    ;; lines. We cant use it for buffers with hidden links.
    (whitespace-mode -1))
  (when (and (equal major-mode 'markdown-mode)
             (not (region-active-p)))
    (custom/hide-all-links)))

;; (cancel-function-timers 'custom/show-link-on-hover)
(run-with-idle-timer 0.1 t 'custom/show-link-on-hover)
;; (cancel-function-timers 'custom/idle-hide-all-links)
(run-with-idle-timer 0.1 t 'custom/idle-hide-all-links)
