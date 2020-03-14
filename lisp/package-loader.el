(defalias
  'custom/install-package-when-needed
  'sysdef/load-external-system)



(load "f__.el")
(load "s__.el")
(load "browse-kill-ring__.el")
(load "pcre2el__.el")
(load "visual-regex__.el")
(load "adaptive-wrap__.el")
(load "spacemacs-theme__.el")
(load "doom-themes__.el")
(load "smart-mode-line__.el")
(load "ace-jump-mode__.el")
(load "ace-window__.el")
(load "autopair__.el")
(load "dired__.el")
(load "elisp-slime-nav__.el")
(load "idle-highlight__.el")
(load "ido__.el")
(load "minimap__.el")
(load "neotree__.el")
(load "paredit__.el")
(load "projectile__.el")
(load "undo-tree__.el")
(load "company__.el")
(load "imenu-list__.el")
(load "semantic__.el")
(load "multiple-cursors__.el")
(load "magit__.el")
(load "ivy__.el")
(load "flycheck__.el")
(load "elpy__.el")
(load "js2-mode__.el")
(load "markdown-mode__.el")
(load "tern__.el")
(load "web-mode__.el")
(load "less-mode__.el")
(load "highlight-indentation__.el")
(load "nlinum__.el")
(load "aggressive-indent__.el")
(load "flyspell__.el")
(load "expand-region__.el")
(load "which-key__.el")
(load "too-long-lines-mode__.el")
(load "multi-term__.el")
(load "elisp-refs__.el")
(load "helpful__.el")
(load "cmake-mode__.el")
(load "fast-scroll__.el")

(require 'font-lock+)
(dolist (symbol '(put-text-property-unless-ignore
                  font-lock-default-unfontify-region
                  font-lock-prepend-text-property
                  font-lock-append-text-property
                  font-lock-fillin-text-property
                  font-lock-apply-syntactic-highlight
                  font-lock-fontify-syntactically-region
                  font-lock-apply-highlight
                  font-lock-fontify-anchored-keywords
                  font-lock-fontify-keywords-region))
  (byte-compile symbol))
