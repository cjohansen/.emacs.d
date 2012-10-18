;; My keybindings for paredit

(require 'paredit)
(require 'bang)

(defun setup-paredit-for-mode-map (mode-map)
  (define-key mode-map (kbd "s-<up>") 'paredit-raise-sexp)
  (define-key mode-map (kbd "s-<right>") 'paredit-forward-slurp-sexp)
  (define-key mode-map (kbd "s-8") 'paredit-wrap-round)
  (define-key mode-map (kbd "s-t") 'transpose-sexps))

(eval-after-load "lisp-mode" '(setup-paredit-for-mode-map emacs-lisp-mode-map))
(eval-after-load "clojure-mode" '(setup-paredit-for-mode-map clojure-mode-map))

(provide 'setup-paredit)
