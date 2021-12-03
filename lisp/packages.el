(require 'cl)


(setq *custom/packages-already-refreshed* nil)
(loop for package in (list 'f 's 'flycheck 'browse-kill-ring 'pcre2el
                           'visual-regexp 'visual-regexp-steroids
                           'adaptive-wrap 'spacemacs-theme
                           'doom-themes 'smart-mode-line
                           'smart-mode-line-powerline-theme
                           'avy 'ace-window 'dired-imenu
                           'dired-subtree 'dash 'elisp-slime-nav
                           'idle-highlight-mode 'ido-vertical-mode
                           'flx-ido 'ido-completing-read+
                           'neotree 'paredit 'projectile 'undo-tree
                           'company 'company-c-headers 'company-web
                           'company-statistics 'imenu-list
                           'multiple-cursors 'magit 'gited 'ivy
                           'company-jedi 'js2-mode 'markdown-mode
                           'tern 'dash 'dash-functional 'web-mode
                           'less-css-mode 'highlight-indentation
                           'aggressive-indent 'expand-region
                           'which-key 'multi-term 'elisp-refs
                           'helpful 'cmake-mode)
      if (not (package-installed-p package))
      do (progn (message "Installing %s" package)
                (when (not *custom/packages-already-refreshed*)
                  (package-refresh-contents)
                  (setq *custom/packages-already-refreshed* t))
                (package-install package)))


(require 'f)
(require 's)
(require 'flycheck)
(require 'flyspell)
(require 'browse-kill-ring)
(require 'pcre2el)
(require 'visual-regexp)
(require 'visual-regexp-steroids)
(require 'adaptive-wrap)
(require 'doom-themes)
(require 'smart-mode-line)
(require 'avy)
(require 'ace-window)
(require 'dired-aux)
(require 'dired-x)
(require 'wdired)
(require 'dired+)
(require 'dired-subtree)
(require 'dired-imenu)
(require 'xref)
(require 'idle-highlight-mode)
(require 'ido)
(require 'ido-vertical-mode)
(require 'ido-completing-read+)
(require 'neotree)
(require 'paredit)
(require 'projectile)
(require 'cl)
(require 'undo-tree)
(require 'company)
(require 'company-c-headers)
(require 'company-web-html)
(require 'company-statistics)
(require 'imenu)
(require 'imenu-list)
(require 'multiple-cursors)
(require 'magit)
(require 'gited)
(require 'ivy)
(require 'python)
(require 'jedi-core)
(require 'js2-mode)
(require 'markdown-mode)
(require 'tern)
(require 'company-tern)
(require 'web-mode)
(require 'less-css-mode)
(require 'highlight-indentation)
(require 'aggressive-indent)
(require 'expand-region)
(require 'which-key)
(require 'multi-term)
(require 'elisp-refs)
(require 'helpful)
(require 'cmake-mode)
(require 'company-cmake)
(require 'hi-lock)


;;;;;;;;;;;;;;;;;;;;;;; Flycheck
(setq flycheck-display-errors-function nil)

;;;;;;;;;;;;;;;;;;;;;;; Flyspell
(ispell-change-dictionary "english" "globally")

;;;;;;;;;;;;;;;;;;;;;;; pcre2el

;; enhance occur and projectile-occur by normal regexes
;; try to enhance isearch too, it will be extremely handy
;; https://github.com/joddie/pcre2el

;;;;;;;;;;;;;;;;;;;;;;; visual-regexp

(defun vr--set-target-buffer-start-end ()
  (setq vr--target-buffer-start (if (region-active-p)
                                    (region-beginning)
                                  (point-min)))
  (setq vr--target-buffer-end (if (region-active-p)
                                  (region-end)
                                (point-max))))

;;;;;;;;;;;;;;;;;;;;;;; adaptive-wrap

(setq-default adaptive-wrap-extra-indent 2)
(adaptive-wrap-prefix-mode 1)

;;;;;;;;;;;;;;;;;;;;;;; doom-themes

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;;;;;;;;;;;;;;;;;;;;;;; smart-mode-line

(setq sml/name-width 60
      sml/mode-width 'full
      sml/no-confirm-load-theme t
      sml/theme nil)

(sml/setup)
(sml/apply-theme 'powerline)

(custom-set-faces
 `(sml/prefix ((t :foreground "lightgray")))
 `(sml/folder ((t :foreground "lightgray")))
 `(sml/position-percentage ((t :foreground "lightgray")))
 `(sml/vc ((t :foreground "lightgray")))
 `(sml/vc-edited ((t :foreground "gold")))
 `(sml/modes ((t :foreground "lightgray"))))

(setq mode-line-modes
      (loop for desc in mode-line-modes
            if (not (equal desc '(:eval (sml/generate-minor-modes))))
            collect desc))

;; TODO:
;; Try modeline from doom mode.
;; https://github.com/seagle0128/doom-modeline

;;;;;;;;;;;;;;;;;;;;;;; AVY

(setq avy-all-windows nil
      avy-background nil)

;;;;;;;;;;;;;;;;;;;;;;; autopair

(electric-pair-mode 1)

;;;;;;;;;;;;;;;;;;;;;;; dired

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(setq diredp-bind-problematic-terminal-keys nil
      wdired-allow-to-change-permissions t
      dired-recursive-copies 'top
      dired-recursive-deletes 'top
      dired-dwim-target t)

(diredp-toggle-find-file-reuse-dir 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(defun custom/dired/create-dir ()
  (interactive)
  (call-interactively 'dired-create-directory)
  (revert-buffer))

(defun custom/dired/create-dir-or-file ()
  (interactive)
  (let ((fname (ido-read-file-name "Create file or directory: "
                                   (dired-current-directory))))
    (if (s-ends-with? custom/fs-separator fname)
        (progn
          (dired-create-directory fname)
          (revert-buffer))
      (progn
        (f-touch fname)
        (revert-buffer)
        (dired-goto-file (expand-file-name fname))))))

(defun custom/dired/get-marked-nodes ()
  (if (equal major-mode 'dired-mode)
      (let ((files (dired-get-marked-files nil nil nil))
            (result nil))
        (dolist (file files)
          (push (list file
                      (f-file-p file)
                      (file-name-extension file))
                result))
        result)
    (error "Not in dired")))

(defun custom/find-free-file-name (file-name)
  (let ((counter 0)
        (run t)
        (new-name nil))
    (while run
      (setq new-name
            (concat file-name
                    ".copy" (format "%s" counter)))
      (if (not (file-exists-p new-name))
          (setq run nil)
        (setq counter (1+ counter))))
    new-name))

(defun dired-create-files (file-creator operation fn-list name-constructor
                                        &optional marker-char)
  (let (dired-create-files-failures failures
                                    skipped (success-count 0) (total (length fn-list)))
    (let (to overwrite-query
             overwrite-backup-query)	; for dired-handle-overwrite
      (dolist (from fn-list)
        (setq to (funcall name-constructor from))
        (if (equal to from)
            (setq to (custom/find-free-file-name to)))
        (if (not to)
            (setq skipped (cons (dired-make-relative from) skipped))
          (let* ((overwrite (file-exists-p to))
                 (dired-overwrite-confirmed ; for dired-handle-overwrite
                  (and overwrite
                       (let ((help-form '(format-message "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
                         (dired-query 'overwrite-query
                                      "Overwrite `%s'?" to))))
                 ;; must determine if FROM is marked before file-creator
                 ;; gets a chance to delete it (in case of a move).
                 (actual-marker-char
                  (cond  ((integerp marker-char) marker-char)
                         (marker-char (dired-file-marker from)) ; slow
                         (t nil))))
            ;; Handle the `dired-copy-file' file-creator specially
            ;; When copying a directory to another directory or
            ;; possibly to itself or one of its subdirectories.
            ;; e.g "~/foo/" => "~/test/"
            ;; or "~/foo/" =>"~/foo/"
            ;; or "~/foo/ => ~/foo/bar/")
            ;; In this case the 'name-constructor' have set the destination
            ;; TO to "~/test/foo" because the old emacs23 behavior
            ;; of `copy-directory' was to not create the subdirectory
            ;; and instead copy the contents.
            ;; With the new behavior of `copy-directory'
            ;; (similar to the `cp' shell command) we don't
            ;; need such a construction of the target directory,
            ;; so modify the destination TO to "~/test/" instead of "~/test/foo/".
            (let ((destname (file-name-directory to)))
              (when (and (file-directory-p from)
                         (file-directory-p to)
                         (eq file-creator 'dired-copy-file))
                (setq to destname))
              ;; If DESTNAME is a subdirectory of FROM, not a symlink,
              ;; and the method in use is copying, signal an error.
              (and (eq t (car (file-attributes destname)))
                   (eq file-creator 'dired-copy-file)
                   (file-in-directory-p destname from)
                   (error "Cannot copy `%s' into its subdirectory `%s'"
                          from to)))
            (condition-case err
                (progn
                  (funcall file-creator from to dired-overwrite-confirmed)
                  (if overwrite
                      ;; If we get here, file-creator hasn't been aborted
                      ;; and the old entry (if any) has to be deleted
                      ;; before adding the new entry.
                      (dired-remove-file to))
                  (setq success-count (1+ success-count))
                  (message "%s: %d of %d" operation success-count total)
                  (dired-add-file to actual-marker-char))
              (file-error		; FILE-CREATOR aborted
               (progn
                 (push (dired-make-relative from)
                       failures)
                 (dired-log "%s `%s' to `%s' failed:\n%s\n"
                            operation from to err))))))))
    (cond
     (dired-create-files-failures
      (setq failures (nconc failures dired-create-files-failures))
      (dired-log-summary
       (format "%s failed for %d file%s in %d requests"
               operation (length failures)
               (dired-plural-s (length failures))
               total)
       failures))
     (failures
      (dired-log-summary
       (format "%s failed for %d of %d file%s"
               operation (length failures)
               total (dired-plural-s total))
       failures))
     (skipped
      (dired-log-summary
       (format "%s: %d of %d file%s skipped"
               operation (length skipped) total
               (dired-plural-s total))
       skipped))
     (t
      (message "%s: %s file%s"
               operation success-count (dired-plural-s success-count)))))
  (revert-buffer))

;;;;;;;;;;;;;;;;;;;;;;; idle-highlight-mode

(add-hook 'prog-mode-hook
          (lambda ()
            (idle-highlight-mode t)))

(setq idle-highlight-idle-time 1)

(advice-add #'idle-highlight-word-at-point
            :around (lambda (oldfn &rest args)
                      (ignore-errors
                        (apply oldfn args))))

;;;;;;;;;;;;;;;;;;;;;;; IDO

(setq ido-vertical-show-count t
      ido-vertical-define-keys 'C-n-C-p-up-down-left-right
      ido-enable-flex-matching t
      flx-ido-use-faces nil)

(ido-mode t)
(ido-everywhere 1)
(ido-vertical-mode 1)
(flx-ido-mode 1)
(ido-ubiquitous-mode 1)

;;;;;;;;;;;;;;;;;;;;;;; neotree

(setq neo-smart-open t
      neo-theme 'ascii
      neo-confirm-change-root 'off-p
      neo-autorefresh nil
      neo-create-file-auto-open nil
      neo-show-hidden-files t
      custom/neotree/last-buffer nil)

(defun custom/neotree-update-root ()
  (ignore-errors
    (let* ((buff-name-raw (buffer-name))
           (buff-name (if (stringp buff-name-raw) buff-name-raw "  ")))
      (if (not (or (string= (substring buff-name 0 1) "*")
                   (string= (substring buff-name 0 2) " *")
                   (string= buff-name custom/neotree/last-buffer)
                   (not (neo-global--window-exists-p))))
          (progn
            (setq custom/neotree/last-buffer buff-name)
            (let ((proj-root (custom/get-project-root)))
              (if (and (stringp proj-root)
                       (not (equal (expand-file-name
                                    (substitute-in-file-name proj-root))
                                   neo-buffer--start-node)))
                  (progn
                    (push-mark nil t nil)
                    (neotree-dir proj-root)
                    (pop-global-mark)))))))))

(run-with-idle-timer 0.5 t 'custom/neotree-update-root)

;;;;;;;;;;;;;;;;;;;;;;; paredit

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook
          #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

(setf (cdr paredit-mode-map) nil)

;;;;;;;;;;;;;;;;;;;;;;; projectile

(setq projectile-tags-backend 'auto
      projectile-track-known-projects-automatically nil
      projectile-switch-project-action 'projectile-find-file)

(projectile-mode)

(cl-defun projectile-completing-read (prompt choices &key initial-input action)
  "Present a project tailored PROMPT with CHOICES."
  (let ((prompt (projectile-prepend-project-name prompt))
        res)
    (setq res
          (cond
           ((<= (length choices) 5000)
            (ido-completing-read prompt choices nil nil initial-input))
           ((> (length choices) 5000)
            (ivy-read prompt choices
                      :initial-input initial-input
                      :action (prog1 action
                                (setq action nil))
                      :caller 'projectile-completing-read))))
    (if action
        (funcall action res)
      res)))

;; Overriden because actual (24.5.2021) version of projectile contains
;; bug which causes failure during file open.
(defun projectile-verify-file (file)
  "Check whether FILE exists in the current project."
  (when (not (equal file 100))
    (file-exists-p (projectile-expand-root file))))

;;;;;;;;;;;;;;;;;;;;;;; undo-tree

;; TODO: Buggy, sometimes it kill buffer mode initialization.
;;
;; (setq undo-tree-auto-save-history t)
;; (setq undo-tree-history-directory-alist
;;       '(("." . "~/.emacs.d/undo")))

(global-undo-tree-mode 1)

(defalias 'redo 'undo-tree-redo)

;;;;;;;;;;;;;;;;;;;;;;; company

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'company-statistics-mode)

(setq company-minimum-prefix-length 1
      company-idle-delay 2
      company-search-regexp-function 'company-search-flex-regexp
      company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-idle-delay 2
      company-tooltip-limit 10
      company-dabbrev-downcase nil
      company-backends '((company-capf
                          company-files
                          company-keywords)
                         (company-etags
                          company-dabbrev-code
                          company-keywords)
                         company-css
                         company-nxml
                         company-cmake
                         company-files
                         company-xcode
                         company-bbdb
                         company-oddmuse
                         company-dabbrev)
      company-dabbrev-ignore-case nil)

;;;;;;;;;;;;;;;;;;;;;;; imenu-list

;; TODO: Alternative?
;; https://github.com/emacsmirror/imenu-tree/blob/master/imenu-tree.el

(setq-default imenu-list-position 'right
              imenu-list-size 0.2
              imenu-list-auto-resize nil
              imenu-list-focus-after-activation nil
              imenu-list-idle-update-delay 1)

(imenu-list-minor-mode)

;; make sure visual line mode is active
(if (functionp 'global-visual-line-mode)
    (if (not global-visual-line-mode)
        (global-visual-line-mode 1)))

;; Following piece of code was good for something but it started to
;; cause problems with newer imenu-list package so I commented it
;; out.
;;
;; (defun custom/imenu-list-update (oldfun &rest args)
;;   (let* ((old (current-buffer))
;;          (oldwin (get-buffer-window old))
;;          (im (get-buffer imenu-list-buffer-name))
;;          (win (if im (get-buffer-window im))))
;;     (when (and (not (active-minibuffer-window))
;;                im
;;                win)
;;       (switch-to-buffer-other-window im)
;;       (select-window oldwin)
;;       (switch-to-buffer old))
;;     (apply oldfun args)))

;; (advice-add #'imenu-list-update
;;             :around 'custom/imenu-list-update)

;; Advices below are fix for crappy imenu list impl.
;;
;; I used following advice to trace what is selecting imenu list
;; after I jumped into different buffer through jedi mode.
;;
;; (advice-add #'select-window
;;             :after (lambda (&rest args)
;;                      (when (equal (buffer-name (window-buffer (car args)))
;;                                   "*Ilist*")
;;                        (backtrace))))

(setq *active-window* nil)

(advice-add #'imenu-list-update-safe
            :before (lambda (&rest args)
                      (setq *active-window* (selected-window))))

(advice-add #'imenu-list-update-safe
            :after (lambda (&rest args)
                     (select-window *active-window*)))

;;;;;;;;;;;;;;;;;;;;;;; CME

(let ((url "https://raw.githubusercontent.com/consciencia/CME/master/cme-install.el")
      (cme-dir (concat user-emacs-directory "CME")))
  (when (not (file-directory-p cme-dir))
    (with-temp-buffer
      (url-insert-file-contents url)
      (eval-buffer)))
  (add-to-list 'load-path cme-dir))
(require 'cme)
(cme-init)

;;;;;;;;;;;;;;;;;;;;;;; multiple-cursors

(setq mc/always-repeat-command t
      mc/edit-lines-empty-lines 'ignore)

(defun custom/mc/mark-next-like-this (arg)
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-after-point)))
        (if cursor
            (mc/remove-fake-cursor cursor)
          (error "No cursors to be unmarked")))
    (mc/mark-lines arg 'forwards))
  (mc/maybe-multiple-cursors-mode))

(defun custom/mc/mark-prev-like-this (arg)
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-before-point)))
        (if cursor
            (mc/remove-fake-cursor cursor)
          (error "No cursors to be unmarked")))
    (mc/mark-lines arg 'backwards))
  (mc/maybe-multiple-cursors-mode))

(setq mc--default-cmds-to-run-once
      (append mc--default-cmds-to-run-once
              '(custom/mc/mark-next-like-this
                custom/mc/mark-prev-like-this)))

(setq *cutom/bulk-clipboard* nil)

(mc/load-lists)
(push 'custom/copy-across-cursors
      mc/cmds-to-run-once)
(push 'cua-copy-region
      mc/cmds-to-run-once)

(defun custom/copy-across-cursors ()
  (interactive)
  (mc/save-excursion
   (mc/save-window-scroll
    (mc/for-each-fake-cursor
     (save-excursion
       (custom/copy-across-cursor cursor)))))
  (mc--reset-read-prompts)
  (push (buffer-substring
         (caar (region-bounds))
         (cdar (region-bounds)))
        *cutom/bulk-clipboard*)
  (kill-new (s-chomp (loop for x in *cutom/bulk-clipboard*
                           concat (concat x "\n"))))
  (setq *cutom/bulk-clipboard* nil))

(defun custom/copy-across-cursor (cursor)
  (let ((mc--executing-command-for-fake-cursor t)
        (id (overlay-get cursor 'mc-id))
        (annoying-arrows-mode nil)
        (smooth-scroll-margin 0))
    (mc/add-fake-cursor-to-undo-list
     (mc/pop-state-from-overlay cursor)
     (ignore-errors
       (custom/cursor-handle-copy)
       (mc/create-fake-cursor-at-point id)))))

(defun custom/cursor-handle-copy ()
  (when (region-active-p)
    (push (buffer-substring
           (caar (region-bounds))
           (cdar (region-bounds)))
          *cutom/bulk-clipboard*))
  (when deactivate-mark (deactivate-mark)))

(advice-add #'cua-copy-region
            :around (lambda (oldfn &rest args)
                      (if (> (mc/num-cursors) 1)
                          (custom/copy-across-cursors)
                        (apply oldfn args))))

;;;;;;;;;;;;;;;;;;;;;;; magit

(setq magit-completing-read-function 'magit-ido-completing-read)

(run-with-idle-timer 5 t #'custom/try-to-use-smerge)

;;;;;;;;;;;;;;;;;;;;;;; python

(advice-add #'jedi:find-file
            :before (lambda (&rest args)
                      (custom/universal-push-mark)))

(advice-add #'jedi:goto--line-column
            :after (lambda (&rest args)
                     (pulse-momentary-highlight-one-line (point))
                     (recenter)))

(advice-add #'jedi:goto-definition--nth
            :around
            (lambda (oldfn other-window &optional try-next)
              (let* ((entries (loop for entry in jedi:goto-definition--cache
                                    for min-entry = (cl-destructuring-bind
                                                        (&key line_nr
                                                              column
                                                              module_path
                                                              module_name
                                                              &allow-other-keys)
                                                        entry
                                                      (list module_name
                                                            module_path
                                                            line_nr
                                                            column))
                                    if (not (string= (car min-entry) "__builtin__"))
                                    if (file-exists-p (cadr min-entry))
                                    collect min-entry))
                     (summarizer (lambda (entry)
                                   (concat (nth 1 entry)
                                           ":"
                                           (format "%s" (nth 2 entry)))))
                     (chosen-entry (custom/ido-completing-read-ctx entries
                                                                   summarizer)))
                (setq jedi:goto-definition--cache nil)
                (when (null chosen-entry)
                  (error "Failed to find definition (%s %s)!"
                         (length jedi:goto-definition--cache)
                         (length entries)))
                (jedi:find-file (nth 1 chosen-entry)
                                (nth 2 chosen-entry)
                                (nth 3 chosen-entry)
                                other-window)
                (run-hooks 'jedi:goto-definition-hook))))

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-jedi company-files))
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))
            (when (not (eq system-type 'windows-nt))
              (flyspell-prog-mode))
            (jedi:setup)
            (flycheck-mode)))

(defun custom/python/indent-or-complete (arg)
  (interactive "P")
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((memq indent-line-function
          '(indent-relative indent-relative-maybe))
    (company-complete-common))
   ((let ((tab-always-indent t)
          (candidates nil))
      (if (and (looking-back (pcre-to-elisp/cached
                              "\\s+")
                             nil)
               (not (looking-back (pcre-to-elisp/cached
                                   "(?:if|in|as|with)\\s")
                                  nil)))
          (indent-for-tab-command arg)
        (progn
          (ignore-errors
            (when (company-manual-begin)
              (setq candidates company-candidates)
              (company-abort)))
          (if (> (length candidates) 0)
              (company-complete-common)
            (indent-for-tab-command arg))))))))

(defun custom/py-normalize-region ()
  "If the first or last line are not fully
selected, select them completely."
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char beg)
    (beginning-of-line)
    (push-mark (point) nil t)
    (goto-char end)
    (unless (= (point) (line-beginning-position))
      (end-of-line))))

(defun custom/py-indent-shift-right (&optional count)
  "Shift current line by COUNT columns to the right.

COUNT defaults to `python-indent-offset'.
If region is active, normalize the region and shift."
  (interactive)
  (if (use-region-p)
      (progn
        (custom/py-normalize-region)
        (python-indent-shift-right (region-beginning)
                                   (region-end)
                                   current-prefix-arg))
    (python-indent-shift-right (line-beginning-position)
                               (line-end-position)
                               current-prefix-arg)))

(defun custom/py-indent-shift-left (&optional count)
  "Shift current line by COUNT columns to the left.

COUNT defaults to `python-indent-offset'.
If region is active, normalize the region and shift."
  (interactive)
  (if (use-region-p)
      (progn
        (custom/py-normalize-region)
        (python-indent-shift-left (region-beginning)
                                  (region-end)
                                  current-prefix-arg))
    (python-indent-shift-left (line-beginning-position)
                              (line-end-position)
                              current-prefix-arg)))

;;;;;;;;;;;;;;;;;;;;;;; js2-mode

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;; markdown-mode

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(defun custom/markdown-file-opened ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       'custom/line-has-hidden-link)
  (flyspell-mode)
  (custom/hide-all-links)
  (local-set-key (kbd "M-l") 'custom/find-all-link-ranges)
  (local-set-key (kbd "M-t") 'custom/wrap-in-more-info))
(add-hook 'markdown-mode-hook 'custom/markdown-file-opened)

(defun custom/wrap-in-more-info ()
  (interactive)
  (beginning-of-visual-line)
  (insert "More info: ")
  (end-of-visual-line)
  (insert "."))

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

(defun custom/find-all-link-ranges (&optional from-pos to-pos)
  (interactive)
  (setq from-pos (or from-pos (point-min))
        to-pos (or to-pos (point-max)))
  (save-excursion
    (goto-char from-pos)
    (let ((regexp (pcre-to-elisp/cached "https?:\\/\\/\\S+"))
          (matches nil)
          (match nil)
          (match-str nil))
      (while (search-forward-regexp regexp to-pos t)
        (setq match (cons (match-beginning 0)
                          (match-end 0)))
        (setq match-str
              (buffer-substring-no-properties (car match)
                                              (cdr match)))
        (cond
         ((save-excursion
            (goto-char (- (cdr match) 2))
            (looking-at (pcre-to-elisp/cached "\\)\\)")))
          (setf (cdr match)
                (- (cdr match) 1)))
         ((and
           (save-excursion
             (goto-char (- (cdr match) 2))
             (looking-at (pcre-to-elisp/cached "\\)[\\.,]")))
           (not (s-match "(" match-str)))
          (setf (cdr match)
                (- (cdr match) 2)))
         ((and (save-excursion
                 (goto-char (1- (cdr match)))
                 (looking-at (pcre-to-elisp/cached "\\)")))
               (not (s-match "(" match-str)))
          (setf (cdr match)
                (1- (cdr match))))
         ((save-excursion
            (goto-char (1- (cdr match)))
            (looking-at (pcre-to-elisp/cached "\\.")))
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

(defun custom/hide-all-links (&optional from-pos to-pos)
  (interactive)
  (when (interactive-p)
    (setq *custom/links-should-be-seen* nil))
  (loop for (link-start . link-end) in (custom/find-all-link-ranges
                                        from-pos
                                        to-pos)
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

(setq *custom/links-should-be-seen* nil)
(defun custom/show-all-links ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'hidden-link-marker)
        (delete-overlay ov)))
    (setq *custom/links-should-be-seen* t)))

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
             (not (region-active-p))
             (not *custom/links-should-be-seen*))
    (let ((win (selected-window)))
      (custom/hide-all-links (window-start win)
                             (window-end win t)))))

;; (cancel-function-timers 'custom/show-link-on-hover)
(run-with-idle-timer 0.1 t 'custom/show-link-on-hover)
;; (cancel-function-timers 'custom/idle-hide-all-links)
(run-with-idle-timer 0.1 t 'custom/idle-hide-all-links)

;;;;;;;;;;;;;;;;;;;;;;; tern

(setq company-tern-property-marker " <p>")

(tern-mode)

(add-hook 'js-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-tern
                   company-files))
            (tern-mode t)
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))
            (when (not (eq system-type 'windows-nt))
              (flyspell-prog-mode))
            (flycheck-mode)))

(advice-add #'tern-go-to-position
            :before (lambda (&rest args)
                      (custom/universal-push-mark)))

(advice-add #'tern-go-to-position
            :after (lambda (&rest args)
                     (pulse-momentary-highlight-one-line (point))
                     (recenter)
                     (js2-reparse nil)))

(defun custom/tern/generare-generic-loader (proj-root)
  (let ((proj-name
         (read-string "Project name: "
                      (let ((fragments
                             (s-split custom/fs-separator
                                      (file-name-as-directory proj-root))))
                        (nth (- (length fragments) 2)
                             fragments)))))
    `((custom/tern/load-project ,proj-name
                                ,(file-name-as-directory proj-root)))))

(defun custom/tern/load-project (proj-name proj-root)
  (custom/tern/generate-config-file proj-root))

(defun custom/tern/generate-config-file (proj-root)
  (if (not (file-exists-p (concat proj-root
                                  ".tern-project")))
      (f-write-text (concat "{\n"
                            "\t\"plugins\": {"
                            "\n\t\t\"node\": {},"
                            "\n\t\t\"modules\": {"
                            "\n\t\t\t\"load\": \".*\""
                            "\n\t\t},"
                            "\n\t\t\"es_modules\": {},"
                            "\n\t\t\"requirejs\": {"
                            "\n\t\t\t\"baseURL\": \"./\","
                            "\n\t\t\t\"paths\": {}"
                            "\n\t\t}"
                            "\n\t},\n"
                            "\t\"libs\": ["
                            "\n\t\t\"ecmascript\","
                            "\n\t\t\"browser\","
                            "\n\t\t\"underscore\","
                            "\n\t\t\"react\","
                            "\n\t\t\"jquery\""
                            "\n\t]"
                            "\n}")
                    'utf-8
                    (concat proj-root
                            ".tern-project"))))

;;;;;;;;;;;;;;;;;;;;;;; web-mode

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(setq web-mode-markup-indent-offset 4
      web-mode-css-indent-offset 4
      web-mode-code-indent-offset 4
      web-mode-style-padding 4
      web-mode-script-padding 4
      web-mode-enable-auto-pairing t
      web-mode-enable-css-colorization t
      web-mode-enable-block-face t
      web-mode-enable-current-element-highlight t
      web-mode-enable-current-column-highlight t)

(add-hook 'web-mode-hook (lambda ()
                           (set (make-local-variable 'company-backends)
                                '((company-web-html
                                   company-css)))
                           (company-mode t)))

;;;;;;;;;;;;;;;;;;;;;;; less-mode

(add-hook 'less-css-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-css))
            (local-set-key (kbd "<tab>") 'company-indent-or-complete-common)))

;;;;;;;;;;;;;;;;;;;;;;; highlight-indentation

(setq highlight-indentation-hooks
      (loop for x in highlight-indentation-hooks
            if (not (equal (car x) 'window-scroll-functions))
            collect x))

(run-with-idle-timer 0.5 t
                     (lambda ()
                       (when highlight-indentation-mode
                         (highlight-indentation-redraw-window
                          (selected-window)
                          'highlight-indentation-overlay
                          'highlight-indentation-put-overlays-region))))

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (highlight-indentation-mode t)
            (set-face-background 'highlight-indentation-face
                                 "#333333")))

;;;;;;;;;;;;;;;;;;;;;;; aggressive-indent

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'html-mode-hook #'aggressive-indent-mode)
(add-hook 'js-mode-hook #'aggressive-indent-mode)
(add-hook 'c-mode-hook #'aggressive-indent-mode)
(add-hook 'c++-mode-hook #'aggressive-indent-mode)

(setq-local *custom/agressive-indent-disabled* nil)

(push '*custom/agressive-indent-disabled*
      aggressive-indent-dont-indent-if)

(defun custom/toggle-agressive-indent ()
  (interactive)
  (setq *custom/agressive-indent-disabled*
        (not *custom/agressive-indent-disabled*))
  (if *custom/agressive-indent-disabled*
      (message "Agressive indent is disabled")
    (message "Agressive indent is enabled")))

(defun custom/disable-agressive-indent ()
  (interactive)
  (setq *custom/agressive-indent-disabled* t)
  (message "Agressive indent is disabled"))

(defun custom/enable-agressive-indent ()
  (interactive)
  (setq *custom/agressive-indent-disabled* nil)
  (message "Agressive indent is enabled"))

(defun custom/agressive-indent-state ()
  (not *custom/agressive-indent-disabled*))

(defun aggressive-indent-indent-region-and-on (l r)
  "Indent region between L and R, and then some.
Call `aggressive-indent-region-function' between L and R, and
then keep indenting until nothing more happens."
  (interactive "r")
  (let ((p (point-marker))
        was-begining-of-line)
    (set-marker-insertion-type p t)
    (unwind-protect
        (progn
          (unless (= l r)
            (when (= (char-before r) ?\n)
              (cl-decf r)))
          ;; If L is at the end of a line, skip that line.
          (unless (= l r)
            (when (= (char-after l) ?\n)
              (cl-incf l)))
          ;; Indent the affected region.
          (goto-char r)
          (unless (= l r) (funcall aggressive-indent-region-function l r))
          ;; And then we indent each following line until nothing happens.
          ;; BAD CODE
          ;; (forward-line 1)
          ;; (skip-chars-forward "[:blank:]\n\r\xc")
          ;; (let ((base-column (current-column)))
          ;;   (while (and (not (eobp))
          ;;               (not (run-hook-with-args-until-success 'aggressive-indent-stop-here-hook))
          ;;               (aggressive-indent--indent-current-balanced-line base-column))))
          ;; BAD CODE
          ;; BEGIN OF FIX
          (let ((run t)
                (oldpos nil)
                left right)
            (while run
              (forward-line 1)
              (setq left (point))
              (end-of-visual-line)
              (setq right (point))
              (goto-char left)
              (skip-chars-forward "[:blank:]\n\r\xc")
              (setq oldpos (point))
              (funcall aggressive-indent-region-function left right)
              (if (equal oldpos (point))
                  (setq run nil))))
          ;; END OF FIX
          )
      (goto-char p))))

;;;;;;;;;;;;;;;;;;;;;;; too-long-lines-mode

(load "too-long-lines-mode.el")

(setq too-long-lines-threshold 320
      too-long-lines-show-number-of-characters 50)

(too-long-lines-mode)

;;;;;;;;;;;;;;;;;;;;;;; multi-term

(setq multi-term-program "/bin/bash")

(push (cons "C-w" 'kill-buffer)
      term-bind-key-alist)
(push (cons "M-l" 'term-line-mode)
      term-bind-key-alist)

;;;;;;;;;;;;;;;;;;;;;;; elisp-refs

(advice-add #'elisp-refs-visit-match
            :before
            (lambda (&rest args)
              (custom/universal-push-mark)))

(advice-add #'elisp-refs--find-file
            :before
            (lambda (&rest args)
              (custom/universal-push-mark)))

(advice-add #'elisp-refs-symbol
            :before
            (lambda (&rest args)
              (custom/universal-push-mark)))

;; Reworked because old one lacked initial input.
(defun elisp-refs--completing-read-symbol (prompt &optional filter)
  "Read an interned symbol from the minibuffer,
defaulting to the symbol at point. PROMPT is the string to prompt
with.

If FILTER is given, only offer symbols where (FILTER sym) returns
t."
  (let ((filter (or filter (lambda (_) t))))
    (read
     (completing-read prompt
                      (elisp-refs--filter-obarray filter)
                      nil nil (thing-at-point 'symbol) nil
                      (-if-let (sym (thing-at-point 'symbol))
                          (when (funcall filter (read sym))
                            sym))))))

(defun custom/elip-refs-quit ()
  (interactive)
  (kill-this-buffer)
  (custom/universal-pop-mark))

;;;;;;;;;;;;;;;;;;;;;;; helpful

(advice-add #'helpful--all-references
            :before
            (lambda (&rest args)
              (custom/universal-push-mark)))

;; Reworked because original impl was missing faces and packages.
(defun helpful-symbol (symbol)
  "Show help for SYMBOL, a variable, function or macro.

See also `helpful-callable' and `helpful-variable'."
  (interactive
   (list (helpful--read-symbol "Symbol: " #'helpful--bound-p)))
  (let ((c-var-sym (helpful--convert-c-name symbol t))
        (c-fn-sym (helpful--convert-c-name symbol nil))
        (c-?-sym (intern (symbol-name symbol))))
    (cond
     ((and (boundp symbol) (fboundp symbol))
      (if (y-or-n-p
           (format "%s is a both a variable and a callable, show variable?"
                   symbol))
          (helpful-variable symbol)
        (helpful-callable symbol)))
     ((fboundp symbol)
      (helpful-callable symbol))
     ((boundp symbol)
      (helpful-variable symbol))
     ((and c-fn-sym (fboundp c-fn-sym))
      (helpful-callable c-fn-sym))
     ((and c-var-sym (boundp c-var-sym))
      (helpful-variable c-var-sym))
     ((or (featurep c-?-sym)
          (locate-library (symbol-name symbol)))
      (describe-package symbol)
      (switch-to-buffer-other-window "*Help*"))
     ((facep c-?-sym)
      (describe-face c-?-sym)
      (switch-to-buffer-other-window "*Help*"))
     (t
      (user-error "Not bound: %S" symbol)))))

;;;;;;;;;;;;;;;;;;;;;;; cmake-mode

(setq cmake-tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;; fast-scroll

(setq custom/scrolling-flag nil)

(defun custom/started-scrolling (&rest args)
  (setq custom/scrolling-flag t))

(defun custom/stopped-scrolling (&rest args)
  (setq custom/scrolling-flag nil))

(defun custom/around-jit-lock-function (old-fn &rest args)
  (when (or (not custom/scrolling-flag)
            (equal major-mode 'dired-mode)
            (equal major-mode 'magit-status-mode)
            (equal major-mode 'magit-log-mode)
            (equal major-mode 'magit-revision-mode)
            (not (null hi-lock-interactive-patterns)))
    (apply old-fn args)))

(advice-add #'previous-line :before #'custom/started-scrolling)
(advice-add #'next-line :before #'custom/started-scrolling)
(advice-add #'scroll-up :before #'custom/started-scrolling)
(advice-add #'scroll-down :before #'custom/started-scrolling)
(advice-add #'scroll-up-command :before #'custom/started-scrolling)
(advice-add #'scroll-down-command :before #'custom/started-scrolling)
(advice-add #'evil-scroll-up :before #'custom/started-scrolling)
(advice-add #'evil-scroll-down :before #'custom/started-scrolling)
(advice-add #'jit-lock-function :around #'custom/around-jit-lock-function)
(run-with-idle-timer 0.1 t #'custom/stopped-scrolling)
