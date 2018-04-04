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

(defun custom/map/to-alist (map)
  (if (hash-table-p map)
      (let ((acc nil))
        (maphash (lambda (k v)
                   (setq acc (cons (cons k v) acc)))
                 map)
        acc)
    nil))

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
        (if (or (equal major-mode 'c-mode)
                (equal major-mode 'c++-mode))
            (ring-insert semantic-tags-location-ring (point-marker)))
        (if (equal major-mode 'emacs-lisp-mode)
            (xref-push-marker-stack))
        (goto-char (overlay-start position))
        (recenter)
        (pulse-momentary-highlight-one-line position))
       (t
        (if (or (equal major-mode 'c-mode)
                (equal major-mode 'c++-mode))
            (ring-insert semantic-tags-location-ring (point-marker)))
        (if (equal major-mode 'emacs-lisp-mode)
            (xref-push-marker-stack))
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

(defun custom/default-completing-read (question candidates)
  (run-at-time "0.5" nil
               (lambda ()
                 (minibuffer-complete)
                 (switch-to-completions)
                 (isearch-forward)))
  (completing-read-default question candidates))

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
                         (t nil))))
    (if loader-content
        (custom/generate-dir-locals proj-root nil loader-content))))

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
   (t (message "No bind in current context"))))

(defun custom/special-m-return-handler ()
  (interactive)
  (cond
   ((active-minibuffer-window)
    (call-interactively 'ido-magic-forward-char))))

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
  (interactive (list (read--expression "Macroexpand: ")))
  (pp-display-expression (macroexpand-all expression)
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

(defun custom/in-comment ()
  (or (company-in-string-or-comment)
      (custom/in-comment-font-lock)))

(defun custom/in-comment-font-lock ()
  (let ((fontfaces (get-text-property (point) 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar #'(lambda (f)
                      (or (eq f 'font-lock-comment-face)
                          (eq f 'font-lock-comment-delimiter-face)))
                  fontfaces))))

(load "monkey.el")
