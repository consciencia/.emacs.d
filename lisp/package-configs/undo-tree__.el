(custom/install-package-when-needed 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
