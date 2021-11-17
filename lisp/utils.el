(require 'pp)
(require 'json)
(load "monkey.el")


(defun custom/map/create ()
  (make-hash-table :test 'equal))

(defun custom/map/get (key map)
  (gethash key map))

(defun custom/map/set (key val map)
  (puthash key val map))

;; Function s-replace-all not works correctly for all inputs in all
;; versions of emacs. This is workaround for that.
;; This function also handle correctly empty replacement list.
(defun custom/replace-all (replacements str)
  (loop for (from . to) in replacements
        do (setq str (s-replace-regexp from to str)))
  str)

(defun custom/get-buffer (name)
  (loop for buff in (buffer-list)
        for buffname = (buffer-name buff)
        if (equal buffname name) return (cons buffname buff)))

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
  (if (or (string= (substring (buffer-name) 0 2) " *")
          (string= (buffer-name) "*scratch*")
          (string= (buffer-name) "*Ilist*"))
      (message "You can not kill protected BUFFER")
    (progn
      (call-interactively 'kill-buffer))))

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

(defun custom/mark-whole-word ()
  (interactive)
  (let ((wbounds (bounds-of-thing-at-point 'symbol)))
    (unless (consp wbounds)
      (error "No word at point"))
    (goto-char (car wbounds))
    (push-mark (save-excursion
                 (goto-char (cdr wbounds))
                 (point)))
    (activate-mark))
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

(defun custom/mark-comment ()
  (interactive)
  (ignore-errors (er/mark-comment))
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun custom/mark-string ()
  (interactive)
  (ignore-errors
    (when (eq (get-text-property (point) 'face)
              'font-lock-string-face)
      (let ((p (point)))
        (while (eq (get-text-property (point) 'face)
                   'font-lock-string-face)
          (forward-char 1))
        (skip-chars-backward " \n\t\r")
        (set-mark (point))
        (goto-char p)
        (while (eq (get-text-property (point) 'face)
                   'font-lock-string-face)
          (forward-char -1))
        (forward-char 1)
        (skip-chars-forward " \n\t\r"))))
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun custom/mark-defun ()
  (interactive)
  (if (or (eq (get-text-property (point) 'face)
              'font-lock-comment-face)
          (eq (get-text-property (point) 'face)
              'font-lock-comment-delimiter-face))
      (ignore-errors (er/mark-comment))
    (if (eq (get-text-property (point) 'face)
            'font-lock-string-face)
        (custom/mark-string)
      (if (and semantic-mode
               (or (equal major-mode 'c-mode)
                   (equal major-mode 'c++-mode)))
          (if (semantic-current-tag)
              (let* ((tag (semantic-current-tag))
                     (bounds (semantic-tag-bounds tag))
                     (b (car bounds))
                     (e (cadr bounds)))
                (goto-char e)
                (set-mark (point))
                (goto-char b)))
        (let ((old-pos (point))
              (b-pos nil)
              (e-pos nil))
          (save-excursion
            (end-of-defun)
            (setq e-pos (point))
            (beginning-of-defun)
            (setq b-pos (point)))
          (if (and b-pos
                   e-pos
                   (>= old-pos b-pos)
                   (<= old-pos e-pos))
              (er/mark-defun))))))
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun custom/mark-args ()
  (interactive)
  (ignore-errors
    (re-search-backward "(")
    (set-mark (+ (point) 1))
    (forward-sexp)
    (backward-char 1))
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun custom/cleanse-imenu-node-name (name)
  (custom/chain-forms
   (s-replace-regexp (pcre-to-elisp/cached
                      "(.*?)\\s+\\(def\\)")
                     "\\1"
                     name)
   (s-replace-regexp (pcre-to-elisp/cached
                      "(.*?)\\s+\\(class\\)")
                     "\\1")
   (s-replace-regexp (pcre-to-elisp/cached
                      "From: (.*)")
                     "")
   (s-replace-regexp (pcre-to-elisp/cached
                      "[\\(\\{]([^\\{]+)[\\)\\}]")
                     "\\1")))

(defun custom/flatten-imenu-root (prefix root)
  (if (and (consp root)
           (not (consp (cdr root))))
      `(,(cons (concat prefix
                       (if (> (length prefix) 0) "." "")
                       (custom/cleanse-imenu-node-name
                        (car root)))
               (cdr root)))
    (loop for sub-root in (cdr root)
          for new-prefix = (concat prefix
                                   (if (> (length prefix) 0) "." "")
                                   (custom/cleanse-imenu-node-name
                                    (car root)))
          append (custom/flatten-imenu-root new-prefix
                                            sub-root))))


(defun custom/imenu-records ()
  (interactive)
  (let* ((tree (save-excursion
                 (funcall imenu-create-index-function)))
         (records (loop for root in tree
                        append (if (and (consp root)
                                        (not (consp (cdr root))))
                                   `(,(cons (custom/cleanse-imenu-node-name
                                             (car root))
                                            (cdr root)))
                                 (custom/flatten-imenu-root ""
                                                            root)))))
    (if (interactive-p)
        (custom/with-simple-pop-up "*Flat Imenu Dump*"
          (setq kill-on-quit t)
          (loop for (name . _) in records
                do (insert name "\n")))
      records)))

(defun custom/goto-imenu-record (record)
  (let ((target (cdr record)))
    (custom/universal-push-mark)
    (cond ((overlayp target)
           (goto-char (overlay-start target)))
          ((markerp target)
           (goto-char (marker-position target)))
          (t (goto-char target)))
    (recenter)
    (pulse-momentary-highlight-one-line (point))))

(defun ido-goto-symbol (&optional symbol-list)
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (when (or (equal major-mode 'c-mode)
            (equal major-mode 'c++-mode))
    (semantic-fetch-tags))
  (let* ((index (custom/map/create))
         (records (custom/imenu-records))
         labels
         label)
    (loop for record in records
          do (progn (custom/map/set (car record)
                                    record
                                    index)
                    (push (car record) labels)))
    (setq label (ido-completing-read "Symbol? " labels))
    (custom/goto-imenu-record (custom/map/get label index))))

(defun custom/scroll-up ()
  (interactive)
  (scroll-up 4))

(defun custom/scroll-down ()
  (interactive)
  (scroll-down 4))

(defun custom/get-simple-input (question opts)
  (interactive)
  (ido-completing-read question opts))

(defun custom/mode ()
  major-mode)

(defun custom/switch-to-minibuffer-window ()
    "switch to minibuffer window (if active)"
    (interactive)
    (when (active-minibuffer-window)
      (select-window (active-minibuffer-window))))

(defun custom/truncate-lines (&optional arg)
  (interactive "P")
  (setq truncate-lines t)
  (force-mode-line-update)
  (unless truncate-lines
    (let ((buffer (current-buffer)))
      (walk-windows (lambda (window)
                      (if (eq buffer (window-buffer window))
                          (set-window-hscroll window 0)))
                    nil t))))

(defun custom/eval (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun custom/projectile-add-known-project (project-root)
  (interactive (list (read-directory-name "Add to known projects: ")))
  (unless (projectile-ignored-project-p project-root)
    (setq projectile-known-projects
          (delete-dups
           (cons (abbreviate-file-name project-root)
                 projectile-known-projects))))
  (projectile-save-known-projects)
  project-root)

(defun custom/project/generate-loader (proj-root proj-type)
  (let ((loader-content (cond
                         ((equal proj-type "Javascript")
                          (custom/tern/generare-generic-loader proj-root))
                         (t nil)))
        (refresher-content nil))
    (if (or loader-content refresher-content)
        (custom/generate-dir-locals proj-root
                                    refresher-content
                                    loader-content))))

(defun custom/special-c-return-handler ()
  (interactive)
  (cond
   ((active-minibuffer-window)
    (call-interactively 'ido-select-text))
   ((equal major-mode 'imenu-list-major-mode)
    (call-interactively 'imenu-list-display-entry))
   ((equal major-mode 'occur-mode)
    (call-interactively 'occur-mode-display-occurrence))
   ((equal major-mode 'grep-mode)
    (call-interactively 'compilation-display-error))
   ((equal major-mode 'compilation-mode)
    (call-interactively 'compilation-display-error))
   ((equal major-mode 'dired-mode)
    (call-interactively 'dired-mark-files-regexp))
   (smerge-mode
    (call-interactively 'smerge-keep-current))
   (with-editor-mode
    (call-interactively 'with-editor-finish))
   ((equal major-mode 'emacs-lisp-mode)
    (call-interactively 'custom/higlight-this))
   ((equal major-mode 'c-mode)
    (call-interactively 'custom/higlight-this))
   ((equal major-mode 'c++-mode)
    (call-interactively 'custom/higlight-this))
   ((equal major-mode 'js-mode)
    (call-interactively 'custom/higlight-this))
   ((equal major-mode 'python-mode)
    (call-interactively 'custom/higlight-this))
   ((equal major-mode 'markdown-mode)
    (call-interactively 'custom/higlight-this))
   ((equal major-mode 'gfm-mode)
    (call-interactively 'custom/higlight-this))
   (t (message "No bind in current context"))))

(defun custom/special-m-return-handler ()
  (interactive)
  (cond
   ((active-minibuffer-window)
    (call-interactively 'ido-magic-forward-char))
   ((equal major-mode 'dired-mode)
    (call-interactively 'dired-unmark-all-marks))
   (smerge-mode
    (call-interactively 'smerge-resolve))
   (with-editor-mode
    (call-interactively 'with-editor-cancel))
   ((equal major-mode 'emacs-lisp-mode)
    (call-interactively 'custom/unhiglight-this))
   ((equal major-mode 'c-mode)
    (call-interactively 'custom/unhiglight-this))
   ((equal major-mode 'c++-mode)
    (call-interactively 'custom/unhiglight-this))
   ((equal major-mode 'js-mode)
    (call-interactively 'custom/unhiglight-this))
   ((equal major-mode 'python-mode)
    (call-interactively 'custom/unhiglight-this))
   ((equal major-mode 'markdown-mode)
    (call-interactively 'custom/unhiglight-this))
   ((equal major-mode 'occur-mode)
    (call-interactively 'custom/occur-mode-display-occurrence))
   (t (message "No bind in current context"))))

(defun custom/enhance-isearch ()
  (define-key isearch-mode-map
    (kbd "C-<right>")
    (lambda ()
      (interactive)
      (call-interactively 'isearch-repeat-forward)
      (recenter)))
  (define-key isearch-mode-map
    (kbd "C-<left>")
    (lambda ()
      (interactive)
      (call-interactively 'isearch-repeat-backward)
      (recenter)))
  (define-key isearch-mode-map (kbd "<next>") 'custom/scroll-up)
  (define-key isearch-mode-map (kbd "<prior>") 'custom/scroll-down)
  (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "C-o")
    (lambda ()
      (interactive)
      (call-interactively 'isearch-occur)
      (switch-to-buffer-other-window "*Occur*")
      (shrink-window-if-larger-than-buffer)))
  (define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char))

(defun custom/generate-dir-locals (path &optional forms guarded-forms)
  (interactive (list (read-directory-name "Where to save dir locals? ")
                     (read (read-string "Enter forms: "))
                     (read (read-string "Enter guarded forms: "))))
  (f-write-text (concat "((nil . ((eval . "
                        (pp-to-string (let ((guard-var (custom/persistent-gensym)))
                                        `(progn
                                           ,@forms
                                           (if (not (boundp ',guard-var))
                                               (progn
                                                 (setq ,guard-var t)
                                                 ,@guarded-forms)))))
                        "))))")
                'utf-8
                (concat (file-name-as-directory path) ".dir-locals.el")))

(defun custom/neotree-startup ()
  (interactive)
  (neotree-show)
  (call-interactively 'other-window))

(defun custom/on-buffer-list-change ()
  (if (not (active-minibuffer-window))
      (custom/truncate-lines)))

(defun custom/full-macroexpand (expression)
  (interactive (list (thing-at-point 'list t)))
  ;; TODO: Modify pp-display-expression to use (custom/with-simple-pop-up)
  (pp-display-expression (macroexpand-all (read expression))
                         "*Macroexpand Output*"))

(defun custom/persistent-gensym (&optional prefix)
  (let ((pfix (if (stringp prefix) prefix "GLOBAL-SYM"))
        (num (custom/db/get "GLOBAL-SYMBOL-COUNTER")))
    (if (not num)
        (setq num 0))
    (custom/db/set "GLOBAL-SYMBOL-COUNTER" (+ num 1))
    (make-symbol (format "%s%d" pfix num))))

(defun custom/nop (&rest args)
  (interactive))

(defun custom/get-project-root ()
  (ignore-errors (projectile-project-root)))

(defun custom/goto-line ()
  (interactive)
  (custom/universal-push-mark)
  (call-interactively 'goto-line)
  (recenter)
  (pulse-momentary-highlight-one-line (point)))

(defun custom/lint-this-buffer ()
  (interactive)
  (flycheck-list-errors)
  (switch-to-buffer-other-window "*Flycheck errors*"))

(defun custom/avy-goto-char-n (&optional arg beg end)
  (interactive (list current-prefix-arg nil nil))
  (avy-with custom/avy-goto-char-n
    (avy-jump (regexp-quote (read-string "Chars: "))
              :window-flip arg
              :beg beg
              :end end)))

(defun custom/avy-jump-char-mode ()
  (interactive)
  (deactivate-mark t)
  (custom/universal-push-mark)
  (call-interactively 'custom/avy-goto-char-n))

(defun custom/universal-push-mark ()
  (interactive)
  (xref-push-marker-stack))

(defun custom/universal-pop-mark ()
  (interactive)
  (deactivate-mark t)
  (let* ((buff (current-buffer))
         (win (selected-window))
         (start (window-start win))
         (end (window-end win)))
    (xref-pop-marker-stack)
    (if (not (and (eq buff (current-buffer))
                  (>= (point) start)
                  (<= (point) end)))
        (recenter))
    (pulse-momentary-highlight-one-line (point))))

(defun custom/define-buffer-key (key func)
  (let ((name (format "%s-magic" (buffer-name))))
    (eval
     `(define-minor-mode ,(intern name)
        "Automagically built minor mode to define buffer-local keys."))
    (let* ((mapname (format "%s-map" name))
           (map (intern mapname)))
      (unless (boundp (intern mapname))
        (set map (make-sparse-keymap)))
      (eval
       `(define-key ,map ,key func)))
    (funcall (intern name) t)))

(defun custom/simple-pop-up (name content)
  (let ((buff (custom/get-buffer name)))
    (if buff
        (setq buff (cdr buff))
      (setq buff (generate-new-buffer name)))
    (with-current-buffer buff
      (read-only-mode -1)
      (erase-buffer)
      (insert content)
      (read-only-mode t))
    (switch-to-buffer-other-window name)
    (shrink-window-if-larger-than-buffer)))

(defmacro custom/with-simple-pop-up (name &rest content)
  (declare (indent 1))
  (let ((buffsym (gensym)))
    `(let ((,buffsym (custom/get-buffer ,name))
           (kill-on-quit nil))
       (if ,buffsym
           (setq ,buffsym (cdr ,buffsym))
         (setq ,buffsym (generate-new-buffer ,name)))
       (with-current-buffer ,buffsym
         (read-only-mode -1)
         (erase-buffer)
         ,@content
         (read-only-mode t))
       (switch-to-buffer-other-window ,name)
       (shrink-window-if-larger-than-buffer)
       (goto-char (point-min))
       (if kill-on-quit
           (local-set-key "q"
                          (lambda ()
                            (interactive)
                            (custom/universal-quit)
                            (kill-buffer ,name)))
         (local-set-key "q" 'custom/universal-quit)))))

(defun custom/list-overlays-at (&optional pos)
  (interactive)
  (setq pos (or pos (point)))
  (let ((overlays (overlays-at pos))
        (obuf (current-buffer))
        (buf (get-buffer-create "*Overlays*"))
        start end text)
    (if (not overlays)
        (message "None.")
      (set-buffer buf)
      (erase-buffer)
      (dolist (o overlays)
        (setq start (overlay-start o)
              end (overlay-end o)
              text (with-current-buffer obuf
                     (buffer-substring start end)))
        (when (> (- end start) 13)
          (setq text (concat (substring text 0 10) "...")))
        (insert (format "From %d to %d: \"%s\":\n" start end text))
        (dolist (p (overlay-properties o))
          (when (overlay-get o p)
            (insert (format " %15S: %S\n" p (overlay-get o p))))))
      (pop-to-buffer buf))))

(defmacro custom/with-measure-time (&rest body)
  (declare (indent 99))
  `(let ((time (current-time)))
     (cons (progn
             ,@body)
           (float-time (time-since time)))))

(defun custom/try-to-use-smerge ()
  (when (and buffer-file-name
             (vc-backend buffer-file-name))
    (when (save-excursion
            (goto-char (point-min))
            (re-search-forward "^<<<<<<< " nil t))
      (if (not smerge-mode)
          (smerge-start-session)))))

(defun custom/flyspell-at-point ()
  (interactive)
  (loop for o in (overlays-at (point))
        if (flyspell-overlay-p o)
        return t))

(defvar-local *custom/flyspell-last-pos* nil)
(defun custom/higlight-this ()
  (interactive)
  (if (or (equal *custom/flyspell-last-pos* (point))
          (custom/flyspell-at-point))
      (progn (flyspell-auto-correct-word)
             (setq *custom/flyspell-last-pos* (point)))
    (progn
      (highlight-regexp (or (find-tag-default-as-symbol-regexp)
                            (regexp-quote (read-string "Highlight: ")))
                        (hi-lock-read-face-name))
      (setq *custom/flyspell-last-pos* nil))))

(defun custom/unhiglight-this ()
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (when (null sym)
      (setq sym (read-string "Unhighlight: ")))
    (setq sym (regexp-quote sym))
    (loop for r
          in (loop for y
                   in (loop for x
                            in hi-lock-interactive-patterns
                            collect (car x))
                   if (s-contains-p sym y)
                   collect y)
          do (hi-lock-unface-buffer r))))

(defun custom/open-environment-settings ()
  (interactive)
  (find-file "~/.bash_profile"))

(defun custom/toggle-uis ()
  (interactive)
  (let ((buff (current-buffer)))
    (imenu-list-smart-toggle)
    (neotree-toggle)
    (switch-to-buffer buff)))

(defun custom/locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret
         (list
          (custom/key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))

(defun custom/key-binding-at-point (key)
  (mapcar (lambda (keymap) (when (keymapp keymap)
                             (lookup-key keymap key)))
          (list
           ;; More likely
           (get-text-property (point) 'keymap)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'keymap))
                   (overlays-at (point)))
           ;; Less likely
           (get-text-property (point) 'local-map)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'local-map))
                   (overlays-at (point))))))

(defun custom/keymaps-at-point ()
  "List entire keymaps present at point."
  (interactive)
  (let ((map-list
         (list
          (mapcar (lambda (overlay)
                    (overlay-get overlay 'keymap))
                  (overlays-at (point)))
          (mapcar (lambda (overlay)
                    (overlay-get overlay 'local-map))
                  (overlays-at (point)))
          (get-text-property (point) 'keymap)
          (get-text-property (point) 'local-map))))
    (apply #'message
           (concat
            "Overlay keymap: %s\n"
            "Overlay local-map: %s\n"
            "Text-property keymap: %s\n"
            "Text-property local-map: %s")
           map-list)))

(defun custom/toggle-camelcase-snakecase ()
  (interactive)
  (save-excursion
    (let* ((first-lower-p t)
           (bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds)))
      (goto-char start)
      (while (equal (char-after (point))
                    (aref "_" 0))
        (right-char))
      (let ((currently-using-underscores-p
             (re-search-forward "_" end t)))
        (if currently-using-underscores-p
            (progn
              (replace-string "_" " " nil start end)
              (upcase-initials-region start end)
              (replace-string " " "" nil start end)
              (when first-lower-p
                (downcase-region start (1+ start))))
          (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
          (downcase-region
           start
           (cdr (bounds-of-thing-at-point 'symbol))))))))

(defun custom/chain-forms-helper (&rest forms)
  (if forms
      (append (car forms)
              (if (cdr forms)
                  (list (apply #'custom/chain-forms-helper
                               (cdr forms)))))))

(defmacro custom/chain-forms (&rest forms)
  (apply #'custom/chain-forms-helper (nreverse forms)))

(defmacro custom/catch-error (do-form err-form)
  (let ((err-sym (gensym)))
    `(condition-case ,err-sym
         (progn ,do-form)
       (error
        (let ((supress-error nil))
          ,err-form
          (if (not supress-error)
              (signal (car ,err-sym)
                      (cdr ,err-sym))))))))

(defun custom/edebug-remove-instrumentation ()
  "Remove Edebug instrumentation from all functions."
  (interactive)
  (let ((functions nil))
    (mapatoms
     (lambda (symbol)
       (when (and (functionp symbol)
                  (get symbol 'edebug))
         (let ((unwrapped (edebug-unwrap* (symbol-function symbol))))
           (unless (equal unwrapped (symbol-function symbol))
             (push symbol functions)
             (setf (symbol-function symbol) unwrapped)))))
     obarray)
    (if (not functions)
        (message "Found no functions to remove instrumentation from")
      (message "Remove edebug instrumentation from %s"
               (mapconcat #'symbol-name functions ", ")))))

(defun custom/val-in-range (val range)
  (when (semantic-tag-p range)
    (setq range
          (cons (semantic-tag-start range)
                (semantic-tag-end range))))
  (and (>= val (car range))
       (<= val (cdr range))))

(defun custom/is-scratch-empty-p ()
  (let ((expected (concat ";; This buffer is for text that is not "
                          "saved, and for Lisp evaluation.\n"
                          ";; To create a file, visit it with C-o and "
                          "enter text in its buffer.\n\n"))
        (actual (with-current-buffer "*scratch*"
                  (buffer-substring-no-properties (point-min)
                                                  (point-max)))))
    (equal expected actual)))

(setq custom/user-said-exit-emacs nil)
(defun custom/can-kill-emacs-p ()
  (or (custom/is-scratch-empty-p)
      (when (yes-or-no-p "SCRATCH buffer not empty, can exit?")
        (setq custom/user-said-exit-emacs t)
        t)))

(add-hook 'kill-emacs-query-functions
          'custom/can-kill-emacs-p)

(advice-add 'kill-emacs :around
            (lambda (fn &rest args)
              (if (and (not (custom/is-scratch-empty-p))
                       (not custom/user-said-exit-emacs))
                  (message "SCRATCH buffer not empty, will not exit!")
                (apply fn args))))

(defun custom/enable-reader-mode ()
  (interactive)
  (visual-line-mode -1)
  (whitespace-mode -1))

(defun custom/disable-reader-mode ()
  (interactive)
  (visual-line-mode 1)
  (whitespace-mode 1))

(defun custom/face-at-point ()
  (interactive)
  (call-interactively 'describe-face))

(defun custom/univeral-defun-name ()
  (interactive)
  (imenu-list-update-safe)
  (let ((entry (imenu-list--current-entry))
        (raw-name nil)
        (capture nil))
    (when entry
      (setq raw-name (car entry)))
    (when raw-name
      (setq capture (or (s-match (pcre-to-elisp/cached
                                  "(.*)\\s\\(def\\)")
                                 raw-name)
                        (s-match (pcre-to-elisp/cached
                                  "\\((.*)\\)")
                                 raw-name))))
    (when capture
      (setq capture (cadr capture)))
    capture))

(defun custom/isearch-forward-defun-name ()
  (interactive)
  (isearch-forward-symbol nil 1)
  (let ((symbol (or (cme-get-current-function-name)
                    (custom/univeral-defun-name))))
    (cond
     (symbol
      (custom/universal-push-mark)
      (isearch-yank-string symbol))
     (t
      (setq isearch-error "Not in function")
      (isearch-push-state)
      (isearch-update)))))

(defun custom/isearch-to-occur ()
  (interactive)
  (call-interactively 'occur)
  (switch-to-buffer-other-window "*Occur*")
  (shrink-window-if-larger-than-buffer))

(defun custom/fontify-whole-buffer ()
  (interactive)
  (save-excursion
    (font-lock-fontify-region (point-min)
                              (point-max))))

(defun cwt (&rest vals)
  (let ((days (loop for val in vals sum val)))
    (insert " "
            (format "%sd/%.0fm"
                    days
                    (* days 8 60)))))

(defmacro custom/regex-generator (type &rest args)
  (cond
   ((equal type 'many)
    `(regexp-opt ',args))
   ((equal type 'this)
    '(custom/univeral-defun-name))
   ((equal type 'pcre)
    `(pcre-to-elisp ,(car args)))
   (t (error "Unknown type '%s'!" type))))
(defalias 're 'custom/regex-generator)

(defun custom/pretty-xml ()
  (interactive)
  (custom/mark-whole-buffer)
  (call-interactively 'sgml-pretty-print))

(defun custom/create-buffer (&optional file-name)
  (interactive)
  (let ((name (or file-name
                  (read-string "Enter filename: "))))
    (generate-new-buffer name)
    (switch-to-buffer-other-window name)))

(defun custom/change-buffer-encoding ()
  (interactive)
  (call-interactively 'set-buffer-file-coding-system))

(defun custom/elisp-macroexpand ()
  (interactive)
  (call-interactively 'custom/full-macroexpand)
  (let ((name "*Macroexpand Output*"))
    (if (get-buffer name)
        (switch-to-buffer-other-window name)
      (message "Failed to perform macroexpand"))))

(defun custom/occur-mode-display-occurrence ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (let ((buffer (current-buffer))
        (pos (occur-mode-find-occurrence))
	    window)
    (setq window (display-buffer (marker-buffer pos) t))
    ;; This is the way to set point in the proper window.
    (save-selected-window
      (select-window window)
      (goto-char pos)
      (recenter)
      (next-error-found buffer (current-buffer))
      (run-hooks 'occur-mode-find-occurrence-hook))))

(defun custom/shell-at-dir ()
  (interactive)
  (let* ((proj-dir (projectile-project-root))
         (dir (if proj-dir
                  proj-dir
                (read-directory-name "Directory: ")))
         (dirbase (file-name-base (s-left -1 dir)))
         (name (concat "*shell-"
                       (s-chop-prefix "." dirbase)
                       "*"))
         (default-directory dir))
    (shell name)))

(defun custom/ido-completing-read-ctx (entries summarise &optional no-error)
  (when (and (not no-error)
             (equal (length entries) 0))
    (error "No entries provided for selection!"))
  (if (equal (length entries) 1)
      (car entries)
    (when (not (equal (length entries) 0))
      (let* ((get-entry-by-summary (lambda (summary summaries)
                                     (loop for (summary2 entry) in summaries
                                           if (equal summary2 summary)
                                           return entry)))
             (seen-summaries (custom/map/create))
             (summaries
              (loop for entry in entries
                    for summary = (funcall summarise entry)
                    unless (prog1
                               (custom/map/get summary seen-summaries)
                             (custom/map/set summary t seen-summaries))
                    collect (list summary entry)))
             (chosen-summary
              (if (equal (length summaries) 1)
                  (caar summaries)
                (ido-completing-read "Choose tag: "
                                     (loop for summary in summaries
                                           collect (car summary)))))
             (chosen-entry (funcall get-entry-by-summary
                                    chosen-summary
                                    summaries)))
        chosen-entry))))
