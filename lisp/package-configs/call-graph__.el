(custom/install-package-when-needed 'call-graph)
(require 'call-graph)

(define-key call-graph-mode-map (kbd "<up>") 'widget-backward)
(define-key call-graph-mode-map (kbd "<down>") 'widget-forward)
