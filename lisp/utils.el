(require 'pp)

(defun custom/map/create ()
  (make-hash-table :test 'equal))

(defun custom/map/get (key map)
  (gethash key map))

(defun custom/map/set (key val map)
  (puthash key val map))

(defun custom/map/len (map)
  (hash-table-count map))

(defun custom/map/clear (map)
  (clrhash map))

(defun custom/map/vectorize (key map)
  (custom/map/set key
                  (apply 'vector
                         (custom/map/get key map))
                  map))

(defun custom/map/to-alist (map)
  (if (hash-table-p map)
      (let ((acc nil))
        (maphash (lambda (k v)
                   (setq acc (cons (cons k v) acc)))
                 map)
        acc)
    nil))



(defun @init (list &optional unsafe)
  (if unsafe
      (nreverse (nthcdr 1 (nreverse list)))
    (nreverse (nthcdr 1 (reverse list)))))

(defalias '@tail 'rest)

(defalias '@head 'car)

(defun @end (list)
  (if list
      (nth (1- (length list)) list)))

(defun @push-back (list val)
  (append list (list val)))

(defalias '@pop-back '@init)

(defalias '@push-front 'cons)

(defalias '@pop-front 'cdr)

(defun @len (list)
  (cond
   ((listp list) (length list))
   ((hash-table-p list) (hash-table-count list))
   (t (error "Bad input to @len"))))

(defun @at (list idx)
  (cond
   ((listp list) (nth idx list))
   ((hash-table-p list) (gethash idx list))
   (t (error "Bad input to @at"))))

(defun @in (list val)
  (cond
   ((listp list) (member val list))
   ((hash-table-p list) (gethash val list))
   (t (error "Bad input to @in"))))

(defun @map (fun list)
  (cond
   ((listp list)
    (loop for x in list collect (funcall fun x)))
   ((hash-table-p list)
    (loop for key being the hash-keys of list using (hash-values val)
          collect (funcall fun key val)))
   (t (error "Bad input to @map"))))

(defalias '@dict-new 'custom/map/create)

(defalias '@dict-set 'custom/map/set)

(defalias '@dict-clear 'custom/map/clear)

(defalias '@dict-val-to-vector 'custom/map/vectorize)

(defalias '@dict-val-to-alist 'custom/map/to-alist)

(defun @dict-& (dict1 dict2))

(defun @dict-| (dict1 dict2))

(defun @dict-^ (dict1 dict2))

(defun @dict-/ (dict1 dict2))



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
      (progn (ignore-errors (er/mark-comment))
             (if (use-region-p)
                 (call-interactively 'cua-copy-region)))
    (if (eq (get-text-property (point) 'face)
            'font-lock-string-face)
        (progn (custom/mark-string)
               (if (use-region-p)
                   (call-interactively 'cua-copy-region)))
      (if (and semantic-mode
               (not (equal major-mode 'python-mode))
               (not (equal major-mode 'lisp-interaction-mode)))
          (if (semantic-current-tag)
              (let* ((bounds (semantic-tag-bounds
                              (semantic-current-tag)))
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
          (if (and b-pos e-pos
                   (>= old-pos b-pos)
                   (<= old-pos e-pos))
              (er/mark-defun))))))
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun custom/flatten-imenu-root (prefix root)
  (if (and (consp root)
           (not (consp (cdr root))))
      `(,(cons (concat prefix
                       (if (> (length prefix) 0) "::" "")
                       (car root))
               (cdr root)))
    (loop for sub-root in (cdr root)
          for new-prefix = (concat prefix
                                   (if (> (length prefix) 0) "::" "")
                                   (car root))
          append (custom/flatten-imenu-root new-prefix
                                            sub-root))))


(defun custom/imenu-records ()
  (interactive)
  (let* ((tree (save-excursion
                 (funcall imenu-create-index-function)))
         (records (loop for root in tree
                        append (if (and (consp root)
                                        (not (consp (cdr root))))
                                   `(,root)
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
            (equal major-mode 'c++-mode)
            (equal major-mode 'emacs-lisp-mode))
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

(setq ECB-ACTIVE nil)
(defun custom/ecb-run ()
  (interactive)
  (ecb-activate)
  (setq ECB-ACTIVE t))

(defun custom/ecb-kill ()
  (interactive)
  (ecb-deactivate)
  (setq ECB-ACTIVE nil))

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

(defun custom/create-imenu-list (new-f)
  (select-frame-set-input-focus new-f)
  (run-at-time "1" nil
               (lambda ()
                 (if (not ECB-ACTIVE) ; (not ecb-minor-mode)
                     (progn
                       (imenu-list-show)
                       (other-window 1))
                   (ecb-redraw-layout-full)))))

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
                         ((equal proj-type "C/C++ (generic)")
                          (custom/ede/generate-generic-loader proj-root))
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

(defun custom/elisp-slime/get-documentation (sym-name)
  (interactive (list (elisp-slime-nav--read-symbol-at-point)))
  (when sym-name
    (let ((sym (intern sym-name)))
      (message "Searching documentation for %s..." sym-name)
      (cond
       ((fboundp sym)
        (describe-function sym)
        (switch-to-buffer-other-window "*Help*"))
       ((boundp sym)
        (describe-variable sym)
        (switch-to-buffer-other-window "*Help*"))
       ((or (featurep sym) (locate-library sym-name))
        (describe-package sym-name)
        (switch-to-buffer-other-window "*Help*"))
       ((facep sym)
        (describe-face sym)
        (switch-to-buffer-other-window "*Help*"))
       (t
        (error "Don't know how to find documentation for '%s'" sym))))))

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

(defun custom/make-link-farm (basePath targetPaths)
  (let* ((base (file-name-as-directory basePath))
         (targets (mapcar #'file-name-as-directory
                          (delete-dups targetPaths)))
         (targetNames (mapcar (lambda (path)
                                (let ((p (s-split custom/fs-separator path)))
                                  (nth (- (length p) 2) p)))
                              targets)))
    (if (or (file-directory-p base)
            (file-exists-p base))
        (delete-directory base t t))
    (make-directory base t)
    (dotimes (i (length targets))
      (let ((target (nth i targets))
            (name (nth i targetNames)))
        (make-symbolic-link target (concat base name) t)))))

(defun custom/nop (&rest args)
  (interactive))

(defun custom/get-project-root ()
  (ignore-errors (projectile-project-root)))

(defun custom/pos-is-in-comment (&optional pos)
  "Returns t when position is in comment. This function
can detect comment via `syntax-ppss' or using test for font
face placed there by font-lock. This function is reasonable
fast so dont be shy.
POS is optional position in file where to search for comment."
  (interactive)
  (if (not pos)
      (setq pos (point)))
  (let ((fontfaces (get-text-property pos 'face))
        (comment-faces '(font-lock-comment-face
                         font-lock-comment-delimiter-face))
        (result nil))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (setq result
          (if font-lock-mode
              (loop for font-face in fontfaces
                    if (memq font-face comment-faces)
                    return t)
            (let ((ppss (syntax-ppss)))
              (or (car (setq ppss (nthcdr 3 ppss)))
                  (car (setq ppss (cdr ppss)))
                  (nth 3 ppss)))))
    (if (called-interactively-p 'any)
        (message "Comment state: %s" result)
      result)))

(defun custom/pos-is-in-string (&optional pos)
  (if (not pos)
      (setq pos (point)))
  (eq (get-text-property pos 'face)
      'font-lock-string-face))

(defun custom/extract-comments-from-region (start stop)
  (when font-lock-mode
    ;; There is possibility that target region is in buffer with font lock
    ;; enabled but with no font locking done yet. In such case, we must
    ;; explicitly fontify that region where we search for comments.
    ;; Of course there is backup for buffers without font locking, but
    ;; this backup is enabled only when font lock mode is disabled so
    ;; its not usable in this situation.
    (font-lock-fontify-region start stop))
  (let ((result "")
        (len (- stop start))
        (finger start)
        (was-in nil)
        (is-in nil))
    (dotimes (finger len result)
      (setq is-in (custom/pos-is-in-comment (+ start finger)))
      (if is-in
          (setq result (concat result
                               (string
                                (char-after (+ start
                                               finger))))))
      (if (and was-in (not is-in))
          (setq result (concat result
                               (if (equal (string
                                           (aref result
                                                 (1- (length result)))) "\n")
                                   "\n"
                                 "\n\n"))))
      (setq was-in is-in))))

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

(defun custom/ace-jump-char-mode ()
  (interactive)
  (deactivate-mark t)
  (custom/universal-push-mark)
  (call-interactively 'avy-goto-char-timer))

(defun custom/universal-push-mark ()
  (if (or (equal major-mode 'python-mode)
          (equal major-mode 'emacs-lisp-mode)
          (equal major-mode 'lisp-interaction-mode)
          (equal major-mode 'c-mode)
          (equal major-mode 'c++-mode))
      (xref-push-marker-stack)
    (if (equal major-mode 'js2-mode)
        (push (cons (buffer-file-name) (point))
              tern-find-definition-stack)
      (xref-push-marker-stack))))

(defun custom/universal-pop-mark ()
  (interactive)
  (deactivate-mark t)
  (if (or (equal major-mode 'python-mode)
          (equal major-mode 'emacs-lisp-mode)
          (equal major-mode 'lisp-interaction-mode)
          (equal major-mode 'c-mode)
          (equal major-mode 'c++-mode))
      (xref-pop-marker-stack)
    (if (equal major-mode 'js2-mode)
        (tern-pop-find-definition)
      (xref-pop-marker-stack)))
  (recenter)
  (pulse-momentary-highlight-one-line (point)))

(defun custom/is-c-file (path)
  (string-match-p ".*\\.\\(c\\|cpp\\|h\\|hpp\\)$"
                  path))

(defun custom/is-js-file (path)
  (string-match-p ".*\\.js$"
                  path))

(defun custom/is-el-file (path)
  (string-match-p ".*\\.el$"
                  path))

(defun custom/is-py-file (path)
  (string-match-p ".*\\.py$"
                  path))

(defun custom/get-files-recur (root predicate)
  (let ((root (file-name-as-directory (file-truename root)))
        (file nil)
        (files (directory-files root t))
        (acc nil))
    (setq files (delete (format "%s." root) files))
    (setq files (delete (format "%s.." root) files))
    (setq files (delete (format "%s.git" root) files))
    (setq files (delete (format "%s.hg" root) files))
    (while files
      (setq file (pop files))
      (if (not (file-accessible-directory-p file))
          (if (apply predicate (list file))
            (setq acc (cons file acc)))
        (setq acc (append acc
                          (custom/get-files-recur file
                                                  predicate)))
        ))
    acc))

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

(defun custom/inspect-eieio (obj)
  (require 'eieio-datadebug)
  (data-debug-new-buffer "*Inspector*")
  (data-debug-insert-object-slots obj ">>"))

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
  (declare (indent 1))
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

(defun custom/get-comment-content (&optional pos)
  (interactive (list (point)))
  (let ((result nil))
    (save-mark-and-excursion
     (goto-char pos)
     (ignore-errors
       (er/mark-comment))
     (if (use-region-p)
         (setq result
               (buffer-substring-no-properties
                (region-beginning)
                (region-end)))))
    result))

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
    (progn (hi-lock-face-symbol-at-point)
           (setq *custom/flyspell-last-pos* nil))))

(defun custom/unhiglight-this ()
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (setq sym (s-replace "+" "\\+" sym))
    (dolist (r (loop for y
                     in (loop for x
                              in hi-lock-interactive-patterns
                              collect (car x))
                     if (s-contains-p sym y)
                     collect y))
      (hi-lock-unface-buffer r))))

(defun custom/open-kekel ()
  (interactive)
  (find-file "~/Documents/Private/jinyKekel"))

(defun custom/open-environment-settings ()
  (interactive)
  (find-file "~/.bash_profile"))

(defun custom/toggle-uis ()
  (interactive)
  (let ((buff (current-buffer)))
    (imenu-list-smart-toggle)
    (neotree-toggle)
    (switch-to-buffer buff)))

(defmacro custom/silence-eldoc-for (func)
  `(advice-add #',func
               :around
               (lambda (oldfun &rest args)
                 (let ((eldoc-message-function
                        #'(lambda (&rest args) nil)))
                   (apply oldfun args)))))

(defmacro custom/silence-eldoc-for-funcs (&rest funcs)
  (cons 'progn
        (loop for func in funcs
              collect `(custom/silence-eldoc-for ,func))))

(custom/silence-eldoc-for-funcs yes-or-no-p
                                y-or-n-p
                                read-from-minibuffer
                                read-string
                                read-regexp
                                save-some-buffers)

(load "monkey.el")

;; (defun custom/python/regex-parser (regex)
;;   (let ((cur 1)
;;         (temp nil)
;;         (res nil))
;;     (dolist (line (s-lines (buffer-substring-no-properties
;;                             (point-min)
;;                             (point-max))))
;;       (setq temp (s-match
;;                   (pcre-to-elisp/cached regex)
;;                   line))
;;       (if temp
;;           (setq res (cons (list (cadr temp) cur) res)))
;;       (setq cur (+ cur 1)))
;;     res))

;; (defun custom/python/parse-tags ()
;;   (interactive)
;;   (append (custom/python/regex-parser "^\\s*def\\s+(\\w+)")
;;           (custom/python/regex-parser "^\\s*class\\s+(\\w+)")
;;           (custom/python/regex-parser "^\\s*import\\s+([\\w\\.]+)")))


;; (defun custom/python/go-to-tag ()
;;   (interactive)
;;   (let ((tags (custom/python/parse-tags))
;;         (res nil))
;;     (setq res (ido-completing-read "Symbol?"
;;                                    (mapcar #'car tags)))
;;     (xref-push-marker-stack)
;;     (goto-line (cadar (seq-filter (lambda (x) (equal res (car x)))
;;                                   tags)))
;;     (pulse-momentary-highlight-one-line (point))))

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

(defun custom/find-string-occurences (string)
  (save-excursion
    (let ((len (length string))
          (result nil))
      (goto-char (point-min))
      (while (search-forward string nil 0)
        (push (cons (- (point) len) (point)) result))
      result)))

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

(defun custom/delete-dups-eq (list)
  "Destructively remove `eq' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `eq' occurrences of an element in LIST, the first
one is kept."
  (let ((hash (make-hash-table :test #'eq :size (length list)))
        (tail list) retail)
    (puthash (car list) t hash)
    (while (setq retail (cdr tail))
      (let ((elt (car retail)))
        (if (gethash elt hash)
            (setcdr tail (cdr retail))
          (puthash elt t hash)
          (setq tail retail)))))
  list)

(defun custom/append-new-backbone (&rest lists)
  (let ((result nil))
    (dolist (list lists)
      (dolist (element list)
        (push element result)))
    result))

(defun custom/longest-string (strings)
  (let ((longest (car strings))
        (head (cdr strings)))
    (while head
      (when (> (length (car head))
               (length longest))
        (setq longest (car head)))
      (setq head (cdr head)))
    longest))

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
  (let ((symbol (or (custom/semantic/get-current-function-name)
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

;; Override because I want to be able to parse number from input with noise.
(defun read-number (prompt &optional default)
  "Read a numeric value in the minibuffer, prompting with PROMPT.
DEFAULT specifies a default value to return if the user just types RET.
The value of DEFAULT is inserted into PROMPT.
This function is used by the `interactive' code letter `n'."
  (let ((n nil)
        (default1 (if (consp default) (car default) default)))
    (when default1
      (setq prompt
            (if (string-match "\\(\\):[ \t]*\\'" prompt)
                (replace-match (format " (default %s)" default1) t t prompt 1)
              (replace-regexp-in-string "[ \t]*\\'"
                                        (format " (default %s) " default1)
                                        prompt t t))))
    (while
        (progn
          (let ((str (read-from-minibuffer
                      prompt nil nil nil nil
                      (when default
                        (if (consp default)
                            (mapcar 'number-to-string (delq nil default))
                          (number-to-string default))))))
            (condition-case nil
                (setq n (cond
                         ((zerop (length str)) default1)
                         ((stringp str)
                          (or (let* ((parser (pcre-to-elisp/cached
                                              "[^\\d]*(\\d+(?:\\.\\d+)?)[^\\d]*"))
                                     (capture (s-match parser str)))
                                (when capture
                                  (read (cadr capture))))
                              (read str)))))
              (error nil)))
          (unless (numberp n)
            (message "Please enter a number.")
            (sit-for 1)
            t)))
    n))

;; Override because I want to be able to execute elisp in regexp input
;; and use its result value as an final input.
(defun occur-read-primary-args ()
  (let* ((perform-collect (consp current-prefix-arg))
         (regexp (read-regexp (if perform-collect
                                  "Collect strings matching regexp"
                                "List lines matching regexp")
                              'regexp-history-last))
         (code (ignore-errors (read regexp))))
    (when (consp code)
      (setq regexp (format "%s" (eval code))))
    (list regexp
          (if perform-collect
              ;; Perform collect operation
              (if (zerop (regexp-opt-depth regexp))
                  ;; No subexpression so collect the entire match.
                  "\\&"
                ;; Get the regexp for collection pattern.
                (let ((default (car occur-collect-regexp-history)))
                  (read-regexp
                   (format "Regexp to collect (default %s): " default)
                   default 'occur-collect-regexp-history)))
            ;; Otherwise normal occur takes numerical prefix argument.
            (when current-prefix-arg
              (prefix-numeric-value current-prefix-arg))))))

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
