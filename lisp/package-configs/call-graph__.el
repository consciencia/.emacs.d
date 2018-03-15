(custom/install-package-when-needed 'call-graph)
(require 'call-graph)

(setq-default cg-initial-max-depth 3
              cg-display-file nil)

(define-key call-graph-mode-map (kbd "<up>") 'widget-backward)
(define-key call-graph-mode-map (kbd "<down>") 'widget-forward)
