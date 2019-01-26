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
        (skip-chars-forward " \n\t\r"))))
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun custom/mark-defun ()
  (interactive)
  (if (eq (get-text-property (point) 'face)
          'font-lock-comment-face)
      (ignore-errors (er/mark-comment))
    (if (eq (get-text-property (point) 'face)
            'font-lock-string-face)
        (custom/mark-string)
      (er/mark-defun)))
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

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
        (custom/universal-push-mark)
        (goto-char (overlay-start position))
        (recenter)
        (pulse-momentary-highlight-one-line position))
       (t
        (custom/universal-push-mark)
        (goto-char position)
        (recenter)
        (pulse-momentary-highlight-one-line position)))))
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
        (refresher-content (cond
                            ((equal proj-type "C/C++ (generic)")
                             (custom/ede/generate-generic-refresher proj-root))
                            (t nil))))
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
  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
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
fast for so dont be shy.
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

(defun custom/ace-jump-word-mode ()
  (interactive)
  (custom/universal-push-mark)
  (call-interactively 'ace-jump-word-mode))

(defun custom/universal-push-mark ()
  (if (or (equal major-mode 'python-mode)
          (equal major-mode 'emacs-lisp-mode)
          (equal major-mode 'c-mode)
          (equal major-mode 'c++-mode))
      (xref-push-marker-stack))
  (if (equal major-mode 'js2-mode)
      (push (cons (buffer-file-name) (point))
            tern-find-definition-stack)))

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

(defun custom/eieo-inspect (obj)
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
    `(let ((,buffsym (custom/get-buffer ,name)))
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
       (goto-char (point-min)))))

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

(defmacro custom/silence-eldoc-for (func)
  `(advice-add #',func
               :around
               (lambda (oldfun &rest args)
                 (let ((backup eldoc-message-function))
                   ;; eat all messages
                   (setq eldoc-message-function
                         (lambda (&rest args) nil))
                   (prog1
                       (apply oldfun args)
                     (setq eldoc-message-function
                           backup))))))

(custom/silence-eldoc-for yes-or-no-p)
(custom/silence-eldoc-for read-from-minibuffer)
(custom/silence-eldoc-for read-string)
(custom/silence-eldoc-for read-regexp)
(custom/silence-eldoc-for save-some-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This hack is used to kill temporary all eldoc
;; reporting when minibuffer is used for something
;; different. Semantic uses eldoc as an interface to
;; present things so this solves issues of eldoc
;; and idle summary mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local *old-eldoc-messager* nil)

(defun custom/eldoc-eater (&rest args)
  nil)

(add-hook 'minibuffer-setup-hook
          (lambda ()
            ;; backup it
            ;; (if (not (equal eldoc-message-function
            ;;                 #'custom/eldoc-eater))
            ;;     (setq *old-eldoc-messager*
            ;;           eldoc-message-function))
            (setq *old-eldoc-messager*
                  eldoc-message-function)
            ;; eat all messages
            (setq eldoc-message-function #'custom/eldoc-eater)))
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (setq eldoc-message-function
                  (or *old-eldoc-messager*
                      #'eldoc-minibuffer-message))))

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
