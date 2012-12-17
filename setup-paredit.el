;; My keybindings for paredit

(require 'paredit)

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(defun setup-paredit-for-mode-map (mode-map)
  (define-key mode-map (kbd "s-<up>") 'paredit-raise-sexp)
  (define-key mode-map (kbd "s-<right>") 'paredit-forward-slurp-sexp)
  (define-key mode-map (kbd "s-<left>") 'paredit-forward-barf-sexp)
  (define-key mode-map (kbd "s-S-<left>") 'paredit-backward-slurp-sexp)
  (define-key mode-map (kbd "s-S-<right>") 'paredit-backward-barf-sexp)
  (define-key mode-map (kbd "s-8") 'paredit-wrap-round)
  (define-key mode-map (kbd "s-9") 'paredit-wrap-round-from-behind)
  (define-key mode-map (kbd "s-<backspace>") 'paredit-splice-sexp-killing-backward)
  (define-key mode-map (kbd "s-t") 'transpose-sexps))

(eval-after-load "lisp-mode" '(setup-paredit-for-mode-map emacs-lisp-mode-map))
(eval-after-load "clojure-mode" '(setup-paredit-for-mode-map clojure-mode-map))

(provide 'setup-paredit)
