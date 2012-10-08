;; My keybindings for paredit

(require 'paredit)

(define-key emacs-lisp-mode-map (kbd "s-<up>") 'paredit-raise-sexp)
(define-key emacs-lisp-mode-map (kbd "s-<right>") 'paredit-forward-slurp-sexp)
(define-key emacs-lisp-mode-map (kbd "s-8") 'paredit-wrap-round)
(define-key emacs-lisp-mode-map (kbd "s-t") 'transpose-sexps)

(provide 'setup-paredit)
