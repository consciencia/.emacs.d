(custom/install-package-when-needed 'git-gutter)
(require 'git-gutter)

(global-git-gutter-mode t)
(git-gutter:linum-setup)

(custom-set-variables
 '(git-gutter:update-interval 2))

(custom-set-variables
 '(git-gutter:modified-sign "  ")
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--"))

(set-face-background 'git-gutter:modified "orange")
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

asdasd
asdasd
asdasd
