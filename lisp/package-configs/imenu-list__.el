(require 'imenu)
(custom/install-package-when-needed 'imenu-list)
(require 'imenu-list)

;; TODO: Alternative?
;; https://github.com/emacsmirror/imenu-tree/blob/master/imenu-tree.el

(setq-default imenu-list-position 'right)
(setq-default imenu-list-size 0.2)
(setq-default imenu-list-auto-resize nil)
(setq-default imenu-list-focus-after-activation nil)

(imenu-list-minor-mode)

;; make sure visual line mode is active
(if (functionp 'global-visual-line-mode)
    (if (not global-visual-line-mode)
        (global-visual-line-mode 1)))

;; make imenu in every new frame
;; this is questionable feature, maybe
;; it will end removed
(add-hook 'after-make-frame-functions
          #'custom/create-imenu-list)


(defun custom|imenu-list-update (oldfun &rest args)
  (let* ((old (current-buffer))
         (oldwin (get-buffer-window old))
         (im (get-buffer imenu-list-buffer-name))
         (win (if im (get-buffer-window im))))
    (when (and (not (active-minibuffer-window))
               im win)
      (switch-to-buffer-other-window im)
      (select-window oldwin)
      (switch-to-buffer old))
    (apply oldfun args)))

(advice-add #'imenu-list-update
            :around 'custom|imenu-list-update)


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
