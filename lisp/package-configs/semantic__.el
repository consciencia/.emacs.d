(custom/install-package-when-needed 'stickyfunc-enhance)
(require 'cc-mode)
(require 'semantic)
(require 'ede)
(require 'cedet-global)
(require 'stickyfunc-enhance)
(require 'semantic/idle)

(global-semantic-idle-scheduler-mode t)
(global-semanticdb-minor-mode t)
(global-semantic-mru-bookmark-mode t)
(global-semantic-stickyfunc-mode -1)
(global-semantic-highlight-edits-mode t)
(global-semantic-show-unmatched-syntax-mode t)
(global-semantic-idle-summary-mode t)
(global-semantic-highlight-func-mode t)
(global-semantic-decoration-mode t)
(global-semantic-idle-breadcrumbs-mode t)

(setq semantic-idle-breadcrumbs-format-tag-function
      'semantic-format-tag-summarize)
(setq semantic-idle-work-parse-neighboring-files-flag t)
(setq semantic-idle-work-update-headers-flag t)

(setq CEDET-GLOBAL-STATE "failed")
(setq cedet-global-command "global")
(if (cedet-gnu-global-version-check t)
   (progn
     (semanticdb-enable-gnu-global-databases 'c-mode)
     (semanticdb-enable-gnu-global-databases 'c++-mode)
     (setq CEDET-GLOBAL-STATE "ok")))

(with-eval-after-load 'semantic
  (add-to-list 'semantic-inhibit-functions
               (lambda ()
                 (if (or (equal major-mode 'c-mode)
                         (equal major-mode 'c++-mode))
                     nil
                   t))))

(semantic-mode 1)
(global-ede-mode 1)
;;(global-srecode-minor-mode 1)



(defvar semantic-tags-location-ring (make-ring 200))

(defun custom/ede/generate-generic-loader (proj-root)
  (let ((root-file (concat (file-name-as-directory proj-root)
                           "PROJLOADER.el")))
    `(progn
       (ede-cpp-root-project ,(read-string "Project name: ")
                             :file ,root-file
                             :include-path '("/include"
                                             "../include")
                             :system-include-path '()
                             :spp-table '()
                             :spp-files '())
       (if (cedet-gnu-global-version-check t) 
           (progn
             (cedet-gnu-global-create/update-database ,proj-root)
             (semantic-symref-detect-symref-tool))
         (progn
           (custom/semantic-index-specific ,proj-root))))))

(defun custom/semantic-goto-definition (point)
  "Goto definition using semantic-ia-fast-jump   
save the pointer marker if tag is found"
  (interactive "d")
  (condition-case err
      (progn                            
        (ring-insert semantic-tags-location-ring (point-marker))  
        (semantic-ia-fast-jump point)
        (recenter))
    (error
     ;;if not found remove the tag saved in the ring  
     (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
     (signal (car err) (cdr err)))))

(defun custom/semantic-switch-proto ()
  "Goto definition using semantic-ia-fast-jump   
save the pointer marker if tag is found"
  (interactive)
  (condition-case err
      (progn                            
        (ring-insert semantic-tags-location-ring (point-marker))  
        (semantic-analyze-proto-impl-toggle)
        (recenter))
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
        (goto-char pos)
        (pulse-momentary-highlight-one-line pos)
        (recenter))                                           
      (set-marker marker nil nil))))

(defun test (&optional arg)
  (message "Arg is %s" arg))

(defun custom/semantic-index-dir-recur (root &optional selection-regex)
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
            (when (string-match-p (if selection-regex
                                      selection-regex
                                    ".*\\.\\(c\\|cpp\\)$")  
                                  file)
              (ignore-errors
                (semanticdb-file-table-object file))))
        ;;else if it's a directory
        (progn
          (semanticdb-save-all-db)
          (custom/semantic-index-dir-recur file))))))

(defun custom/semantic-index-specific (root)
  (interactive)
  (custom/semantic-index-dir-recur root)
  (semanticdb-save-all-db))

(defun custom/semantic-index-this-dir ()
  (interactive)
  (custom/semantic-index-dir-recur default-directory)
  (semanticdb-save-all-db))

(defun custom/semantic-index-this-projectile-project ()
  (interactive)
  (custom/semantic-index-dir-recur (projectile-project-root))
  (semanticdb-save-all-db))

;; inject preprocessor values into semantic
;; (semantic-c-add-preprocessor-symbol "__KERNEL__" "")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add include directory which will then semnatic use for
;; looking for header files, handy when writing some buildsystem
;; scrapper
;; c-mode || c++-mode
;; (semantic-add-system-include include-root-dir symbol-for-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; controll loading of symbol tables
;; semanticdb-find-default-throttle

;; WARNING after emacs update (together with CEDET)
;; you must delete whole semnatic cache becuase it is in invalid
;; format (better said, incompatible with new version) and semantic
;; simply silently fails instead of detecting incompatibility
;;
;; TODO write mail to ericludlam@gmail.com about this
;; critical feature request ...
