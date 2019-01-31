(custom/install-package-when-needed 'undo-tree)

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      '(("." . "~/.emacs.d/undo")))

(global-undo-tree-mode 1)

(defalias 'redo 'undo-tree-redo)
