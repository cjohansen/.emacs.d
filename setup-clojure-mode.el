(require 'clojure-mode)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  (save-buffer))

(require 'clj-refactor)

(cljr-add-keybindings-with-modifier "C-s-")
(define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)

(add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))

(define-key clojure-mode-map (kbd "s-j") 'clj-jump-to-other-file)

(require 'nrepl)

(define-key nrepl-mode-map (kbd "<home>") nil)
(define-key nrepl-mode-map (kbd "C-,") 'complete-symbol)
(define-key nrepl-interaction-mode-map (kbd "C-,") 'complete-symbol)

;; Configure nrepl.el
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl t)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")

;; Some default eldoc facilities
(add-hook 'nrepl-connected-hook
          (defun my-nrepl-eldoc-hook ()
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
            (nrepl-enable-on-existing-clojure-buffers)))

(provide 'setup-clojure-mode)
