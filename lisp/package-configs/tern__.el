;; git clone https://github.com/ternjs/tern.git
;; npm init
(add-to-list 'load-path "/home/box/.emacs.d/tern/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(require 'tern)

(add-hook 'js-mode-hook (lambda () (tern-mode t)))

(tern-mode)

(define-key tern-mode-keymap (kbd "M-.") 'tern-find-definition)
(define-key tern-mode-keymap (kbd "M-,") 'tern-pop-find-definition)
(define-key tern-mode-keymap (kbd "M-*") 'tern-get-docs)

;; ADD JS PROJ TYPE with .tern-config like this in root
;; {
;;   "plugins": {
;;     "node": {},
;;     "es_modules": {}
;;   },
;;   "libs": [
;;     "ecma5",
;;     "ecma6"
;;   ],
;;   "ecmaVersion": 6
;; }
