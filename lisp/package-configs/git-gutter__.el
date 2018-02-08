(custom/install-package-when-needed 'git-gutter)
(require 'git-gutter)

(global-git-gutter-mode t)
(git-gutter:linum-setup)

(custom-set-variables
 '(git-gutter:update-interval 1))

(custom-set-variables
 '(git-gutter:modified-sign "  ")
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--"))

(set-face-background 'git-gutter:modified "red")
(set-face-foreground 'git-gutter:added "white")
(set-face-foreground 'git-gutter:deleted "white")
