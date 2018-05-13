(require 'cl)
(require 'package)


(defstruct sysdef/key-descriptor
  notation
  doc
  owning-map)

(defstruct sysdef/subsystem
  name
  order
  required-external-packages
  is-started
  description
  init-callback
  registered-keys)


(setq *sysdef/main-register* nil)
(setq *sysdef/current-subsystem* nil)


(defun sysdef/find (name-of-subsystem)
  (let ((result nil))
    (dolist (system *sysdef/main-register*)
      (if (and (equal name-of-subsystem (sysdef/subsystem-name system))
               (not result))
          (setq result system)))
    result))

(defun sysdef/add-key-descriptor (name-of-subsystem key)
  (let ((system (if (sysdef/subsystem-p name-of-subsystem)
                    name-of-subsystem
                  (sysdef/find name-of-subsystem))))
    (if (sysdef/subsystem-p system)
        (let ((keys (sysdef/subsystem-registered-keys system)))
          (push key keys)
          (setf (sysdef/subsystem-registered-keys system) keys))
      (error "Not found subsystem %s in central registry"
             name-of-subsystem))))

(defun sysdef/start-subsystems ()
  (sysdef/prepare-external-system-manager)
  (setq *sysdef/main-register*
        (sort *sysdef/main-register*
              (lambda (first second)
                (< (sysdef/subsystem-order first)
                   (sysdef/subsystem-order second)))))
  (dolist (system *sysdef/main-register*)
    (sysdef/load-external-systems
     (sysdef/subsystem-required-external-packages system))
    (apply (sysdef/subsystem-init-callback system) nil)
    (setf (sysdef/subsystem-is-started system) t)))

(defun sysdef/prepare-external-system-manager ()
  (setq package-check-signature nil)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/")))

(defun sysdef/load-external-system (system-name)
  (unless (package-installed-p system-name)
    (progn (message "installing %s" system-name)
           (package-refresh-contents)
           (package-install system-name))))

(defun sysdef/load-external-systems (system-names)
  (dolist (system-name system-names)
    (sysdef/load-external-system system-name)))

(defmacro with-subsystem (subsystem-name &rest code)
  `(let ((*sysdef/current-subsystem* (sysdef/find ',subsystem-name)))
     (if (not *sysdef/current-subsystem*)
         (error "Not found subsystem %s" ',subsystem-name))
     ,@code))

;; (defsubsystem name-of-system 0 nil
;;   "Doc string which may spawn multiple
;; lines"
;;   (some lisp)
;;   (code here)
;;   (and so on))
(defmacro defsubsystem (subsystem-name order ext-packages  doc &rest init-code)
  `(push (make-sysdef/subsystem :name ',subsystem-name
                                :order ,order
                                :required-external-packages ',ext-packages
                                :is-started nil
                                :description ,doc
                                :init-callback (lambda ()
                                                 (with-subsystem ,subsystem-name
                                                                 ,@init-code))
                                :registered-keys nil)
         *sysdef/main-register*))

;; (defkey "Doc string about key"
;;   some-keymap-or-nil
;;   "M-d"
;;   'mark-defun)
(defmacro defkey (doc key-map keybind handler)
  `(progn
     (sysdef/add-key-descriptor *sysdef/current-subsystem*
                                (make-sysdef/key-descriptor :notation ,keybind
                                                            :doc ,doc
                                                            :owning-map ,key-map))
     ,(if key-map
         `(define-key ,key-map (kbd ,keybind) ,handler)
       `(global-set-key (kbd ,keybind) ,handler))))

(provide 'sysdef)
