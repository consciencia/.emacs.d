(require 'flyspell)

(define-key flyspell-mode-map "\M-\t" nil)
(define-key flyspell-mode-map flyspell-auto-correct-binding nil)
(define-key flyspell-mode-map [(control ?\,)] nil)
(define-key flyspell-mode-map [(control ?\.)] nil)
(define-key flyspell-mode-map [?\C-c ?$] nil)
