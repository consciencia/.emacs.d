(defun custom/universal-quit ()
  (interactive)
  (let* ((this-window (selected-window))
         (this-buffer (window-buffer this-window))
         (this-buffer-name (buffer-name this-buffer))
         (win-count (length (window-list)))
         (frame-count (length (frame-list))))
    (if (equal this-buffer-name " *NeoTree*")
        (message "Can not close protected window")
      (if (> win-count 1)
          (progn
            (if (only-minimal-default-ui-component-shown)
                (if (> frame-count 1)
                    (delete-frame)  
                  (save-buffers-kill-emacs))
              (progn
                (delete-window this-window)
                (if (only-default-ui-component-shown)
                    (if (> frame-count 1)
                        (delete-frame)  
                      (save-buffers-kill-emacs))))))  
        (if (> frame-count 1)
            (delete-frame)
          (save-buffers-kill-emacs))))))

(defun custom/kill-buffer ()
  (interactive)
  (if (string= (substring (buffer-name) 0 2) " *")
      (progn
        (message "You can not kill protected BUFFER"))
    (progn
      (if (string= (substring (buffer-name) 0 1) "*")
          (progn
            (call-interactively 'kill-buffer))
        (progn
          (call-interactively 'kill-buffer)
          ;; (run-at-time "0.25 sec" nil
          ;;              (lambda ()
          ;;                (call-interactively 'windmove-right)))
          )))))

(defun y-or-n-p-with-return (orig-func &rest args)
    (let ((query-replace-map (copy-keymap query-replace-map)))
      (define-key query-replace-map (kbd "RET") 'act)
      (apply orig-func args)))

(defun ignore-error-wrapper (fn)
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(defun get-window-active-buffers ()
  (let ((res nil))
    (dolist (win (window-list)) 
      (setq res
            (cons (buffer-name
                   (window-buffer win))
                  res)))
    res))

(defun only-default-ui-component-shown ()
  (let ((winnames (get-window-active-buffers)))
    (if (equal 2 (length winnames))
        (and (member " *NeoTree*" winnames)
             (or (member " *MINIMAP*" winnames)
                 (member "*Ilist*" winnames)))
      (if (equal 1 (length winnames))
          (or (member " *NeoTree*" winnames)
              (member " *MINIMAP*" winnames)
              (member "*Ilist*" winnames))
        nil))))

(defun only-minimal-default-ui-component-shown ()
  (let ((winnames (get-window-active-buffers)))
    (if (and (equal 2 (length winnames))
             (member " *NeoTree*" winnames)
             (not (or (member " *MINIMAP*" winnames)
                      (member "*Ilist*" winnames))))
        t
      nil)))

(defun custom/mark-whole-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (region-active-p)))
      (set-mark
       (save-excursion
         (when (< (mark) (point))
           (setq arg (- arg)))
         (goto-char (mark))
         (forward-word arg)
         (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
        (error "No word at point"))
      (if (>= arg 0)
          (goto-char (car wbounds))
        (goto-char (cdr wbounds)))
      (push-mark (save-excursion
                   (forward-word arg)
                   (point)))
      (activate-mark)))
  ;; Secret sauce for standard selection behavior
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun custom/mark-whole-line ()
  "Select the current line"
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (region-active-p))
      (forward-char))
  (end-of-line) ; move to end of line
  (if (not (or (and (eq last-command this-command) (mark t))
               (region-active-p)))
      (set-mark (line-beginning-position)))
  ;; Secret sauce for standard selection behavior
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun custom/mark-whole-buffer ()
  (interactive)
  (set-mark (point-min))
  (goto-char (point-max))
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun custom-paredit-init ()
  (local-set-key (kbd "(" ) 'paredit-open-round) 
  (local-set-key (kbd ")" ) 'paredit-close-round)
  (local-set-key (kbd "[" ) 'paredit-open-square)
  (local-set-key (kbd "]" ) 'paredit-close-square) 
  (local-set-key (kbd "\"" ) 'paredit-doublequote)
  (local-set-key (kbd ";" ) 'paredit-semicolon)
  (local-set-key (kbd "C-<right>") 'paredit-forward)
  (local-set-key (kbd "C-<left>") 'paredit-backward)
  (local-set-key (kbd "C-<down>") 'paredit-forward-down)
  (local-set-key (kbd "C-<up>") 'paredit-backward-up))

(defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))

(defun custom/c-indent-or-complete ()
  (interactive)
  (let ((old-point (point))
        (old-tick (buffer-chars-modified-tick)))
    (call-interactively 'c-indent-line-or-region)
    (if (and (eq old-point (point))
             (eq old-tick (buffer-chars-modified-tick)))
        (call-interactively 'company-search-candidates))))
