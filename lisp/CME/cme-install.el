(require 'url)


(let* ((root (expand-file-name user-emacs-directory))
       (cme-root (file-name-as-directory (concat root "CME")))
       (db-root (file-name-as-directory (concat root "semanticdb")))
       (root-url "https://raw.githubusercontent.com/consciencia/.emacs.d/master/lisp/CME/")
       (urls (loop for name in '("cme-analyze.el"
                                 "cme-c.el"
                                 "cme-company.el"
                                 "cme-complete.el"
                                 "cme-cpp-root.el"
                                 "cme-db-find.el"
                                 "cme-db.el"
                                 "cme-doc.el"
                                 "cme-ede-generic-proj.el"
                                 "cme-find.el"
                                 "cme-ia.el"
                                 "cme-index.el"
                                 "cme-k.el"
                                 "cme-misc.el"
                                 "cme-refs.el"
                                 "cme-search.el"
                                 "cme-semanticdb-grep.el"
                                 "cme-senator.el"
                                 "cme-symref.el"
                                 "cme-utils.el"
                                 "cme.el")
                   collect (concat root-url name))))
  (when (and (file-directory-p cme-root)
             (y-or-n-p "CME already installed, reinstall?"))
    (delete-directory cme-root t t))
  (make-directory cme-root)
  (make-directory db-root t)
  (loop for url in urls
        do (progn
             (message "Downloading %s" url)
             (url-copy-file url
                            (concat cme-root
                                    (car (last (split-string url "/" t))))))))

;; Installation command.
;;
;; (with-temp-buffer
;;   (require 'url)
;;   (url-retrieve-synchronously "?")
;;   (eval-buffer))
