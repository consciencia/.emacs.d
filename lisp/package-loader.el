(require 'cl)
(require 'package)
(require 'whitespace)

(setq package-check-signature nil)

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(defun custom/install-package-when-needed (package)
  (unless (package-installed-p package)
    (progn (message "installing %s" package)
           (package-refresh-contents)
           (package-install package))))

(load "f__.el")
(load "s__.el")
(load "browse-kill-ring__.el")
(load "pcre2el__.el")
(load "grizzl__.el")
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
(load "shackle__.el")
(load "multiple-cursors__.el")
(load "magit__.el")
(load "git-gutter__.el")
(load "call-graph__.el")
(load "ivy__.el")
(load "flycheck__.el")
(load "elpy__.el")
