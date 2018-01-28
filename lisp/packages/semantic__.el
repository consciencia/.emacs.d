(custom/install-package-when-needed 'stickyfunc-enhance)
(require 'cc-mode)
(require 'semantic)
(require 'cedet-global)
(require 'stickyfunc-enhance)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-mru-bookmark-mode 1)

(setq semanticdb-project-roots projectile-known-projects)

(setq cedet-global-command "global")
(if (cedet-gnu-global-version-check t)
   (progn
     (semanticdb-enable-gnu-global-databases 'c-mode)
     (semanticdb-enable-gnu-global-databases 'c++-mode))
 (progn
   (message "FAILED TO CONNECT SEMANTIC TO GLOBAL")))

(defvar semantic-tags-location-ring (make-ring 200))
(defun custom/semantic-goto-definition (point)
  "Goto definition using semantic-ia-fast-jump   
save the pointer marker if tag is found"
  (interactive "d")
  (condition-case err
      (progn                            
        (ring-insert semantic-tags-location-ring (point-marker))  
        (semantic-ia-fast-jump point))
    (error
     ;;if not found remove the tag saved in the ring  
     (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
     (signal (car err) (cdr err)))))

(defun custom/semantic-pop-tag-mark ()             
  "popup the tag save by semantic-goto-definition"   
  (interactive)                                                    
  (if (ring-empty-p semantic-tags-location-ring)                   
      (message "%s" "No more tags available")                      
    (let* ((marker (ring-remove semantic-tags-location-ring 0))    
           (buff (marker-buffer marker))                        
           (pos (marker-position marker)))                   
      (if (not buff)                                               
          (message "Buffer has been deleted")                    
        (switch-to-buffer buff)                                    
        (goto-char pos))                                           
      (set-marker marker nil nil))))

(defun custom/semantic-index-dir-recur (root)
  (let ((root (file-name-as-directory root))
        (files (directory-files root t)))
    (setq files (delete (format "%s." root) files))
    (setq files (delete (format "%s.." root) files))
    (setq files (delete (format "%s.git" root) files))
    (setq files (delete (format "%s.hg" root) files))
    (while files
      (setq file (pop files))
      (if (not (file-accessible-directory-p file))
          (progn
            (when (string-match-p ".*\\.\\(c\\|cpp\\|h\\|hpp\\)$"
                                  file)
              (ignore-errors
                (semanticdb-file-table-object file))))
        ;;else if it's a directory
        (progn
          (semanticdb-save-all-db)
          (custom/semantic-index-dir-recur file))))))

(defun custom/semantic-index-this-dir ()
  (interactive)
  (custom/semantic-index-dir-recur default-directory)
  (semanticdb-save-all-db))

(defun custom/semantic-index-this-projectile-project ()
  (interactive)
  (custom/semantic-index-dir-recur (projectile-project-root))
  (semanticdb-save-all-db))

;; returns symref tool
;; (semantic-symref-detect-symref-tool)
;; force CEDET to use GLOBAL for symref
;; (setq-default semantic-symref-tool "global")
;; list of function which are able to retrieve project root
;; semanticdb-project-root-functions


