(require 'edebug)

(setq edebug-save-windows nil)

(define-key edebug-mode-map "n" 'edebug-step-mode)
(define-key edebug-mode-map "s" 'edebug-step-in)
(define-key edebug-mode-map "f" 'edebug-step-out)
(define-key edebug-mode-map "c" 'edebug-continue-mode)
(define-key edebug-mode-map "b" 'edebug-set-breakpoint)
(define-key edebug-mode-map "d" 'edebug-unset-breakpoint)
(define-key edebug-mode-map "t" 'edebug-backtrace)
(define-key edebug-mode-map " " 'edebug-goto-here)
