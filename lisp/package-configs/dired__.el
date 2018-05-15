;; Create sidebar for dired
;; with help of code in neo-global--create-window

(custom/install-package-when-needed 'dired-imenu)
(custom/install-package-when-needed 'dired-subtree)
(require 'dired-x)
(require 'dired-imenu)
(require 'dired+)
(require 'dired-subtree)
(require 'xref)

(eval-after-load "dired-aux"
 '(add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(setq diredp-bind-problematic-terminal-keys nil
      dired-recursive-copies 'top
      dired-recursive-deletes 'top
      dired-dwim-target t)
(diredp-toggle-find-file-reuse-dir 1)

(define-key dired-mode-map (kbd "<C-kp-subtract>")
  'diredp-up-directory-reuse-dir-buffer)
(define-key dired-mode-map (kbd "<C-kp-add>")
  (lambda ()
    (interactive)
    (let ((fname (ido-read-file-name "Create file or directory: "
                                     (dired-current-directory))))
      (if (s-ends-with? custom/fs-separator fname)
          (dired-create-directory fname)
        (progn
          (f-touch fname)
          (dired-add-file fname)
          (dired-goto-file (expand-file-name fname)))))))
(define-key dired-mode-map (kbd "<kp-add>") 'dired-create-directory)
(define-key dired-mode-map (kbd "C-<up>") 'dired-prev-dirline)
(define-key dired-mode-map (kbd "C-<down>") 'dired-next-dirline)
(define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
(define-key dired-mode-map (kbd "I") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)

(define-key xref--button-map (kbd "C-<return>")
  'xref-show-location-at-point)

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
