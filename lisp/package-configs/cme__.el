(let ((url "https://raw.githubusercontent.com/consciencia/CME/master/cme-install.el")
      (cme-dir (concat user-emacs-directory "CME")))
  (when (not (file-directory-p cme-dir))
    (with-temp-buffer
      (url-insert-file-contents url)
      (eval-buffer)))
  (add-to-list 'load-path cme-dir))
(require 'cme)
(cme-init)
