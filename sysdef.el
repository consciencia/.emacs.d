(require 'cl)
(require 'package)


(defstruct sysdef/key-descriptor
  notation
  doc
  owning-map)

(defstruct sysdef/system
  name
  dependencies
  status
  description
  init-callback
  registered-keys)


(setq *sysdef/main-register* (make-hash-table :test 'equal))
(setq *sysdef/current-system* nil)


(defun sysdef/find (name-of-system)
  (gethash name-of-system
           *sysdef/main-register*))

(defun sysdef/add-key-descriptor (name-of-system key)
  (let ((system (if (sysdef/system-p name-of-system)
                    name-of-system
                  (sysdef/find name-of-system))))
    (if (sysdef/system-p system)
        (let ((keys (sysdef/system-registered-keys system)))
          (puthash (sysdef/key-descriptor-notation key)
                   key
                   keys)
          (setf (sysdef/system-registered-keys system) keys))
      (error "Not found system %s in central registry"
             name-of-system))))

(defun sysdef/init ()
  (sysdef/init-load-paths)
  (sysdef/init-external-system-manager))

(defun sysdef/start-system (system-name)
  )

(defun sysdef/load-system (system-name)
  (cond
   ((require system-name)
    (ignore-errors
      (sysdef/start-system system-name)))
   ((sysdef/load-external-system system-name)
    (require system-name)
    (ignore-errors
      (sysdef/start-system system-name)))
   (t (sysdef/start-system system-name))))

(defun sysdef/load-systems (system-names)
  (dolist (system-name system-names)
    (sysdef/load-system system-name)))

(defun sysdef/init-load-paths ()
  (add-to-list 'load-path
               (concat (expand-file-name user-emacs-directory)
                       "lisp"))
  (let ((default-directory
          (concat (expand-file-name user-emacs-directory)
                  "lisp")))
    (normal-top-level-add-subdirs-to-load-path)))

(defun sysdef/init-external-system-manager ()
  (setq package-check-signature nil)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/")))

(defun sysdef/load-external-system (system-name)
  (unless (package-installed-p system-name)
    (progn (message "installing %s" system-name)
           (package-refresh-contents)
           (package-install system-name)
           (package-installed-p system-name))))

(defmacro with-system (system-name &rest code)
  `(let ((*sysdef/current-system* (sysdef/find ',system-name)))
     (if (not *sysdef/current-system*)
         (error "Not found subsystem %s" ',system-name))
     ,@code))

;; (defsubsystem name-of-system (semantic my-system and-so-on)
;;   "Doc string which may spawn multiple
;; lines"
;;   (some-lisp)
;;   (code-here)
;;   (and-so-on))
(defmacro defsystem (system-name dependencies doc &rest init-code)
  `(puthash ,system-name
            (make-sysdef/system :name ',system-name
                                :dependencies ',dependencies
                                :status 'not-started
                                :description ,doc
                                :init-callback (lambda ()
                                                 (with-system ,system-name
                                                              ,@init-code))
                                :registered-keys (make-hash-table :test 'equal))
            *sysdef/main-register*))

;; (defkey "Doc string about key"
;;   some-keymap-or-nil
;;   "M-d"
;;   'mark-defun)
(defmacro defkey (doc key-map keybind handler)
  `(progn
     (sysdef/add-key-descriptor *sysdef/current-system*
                                (make-sysdef/key-descriptor :notation ,keybind
                                                            :doc ,doc
                                                            :owning-map ,key-map))
     ,(if key-map
          `(define-key ,key-map (kbd ,keybind) ,handler)
        `(global-set-key (kbd ,keybind) ,handler))))

(provide 'sysdef)
