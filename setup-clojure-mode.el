(require 'clojure-mode)

(define-key clojure-mode-map (kbd "C-c C-j") 'clj-jump-to-other-file)
(define-key clojure-mode-map (kbd "C-c M-j") 'clj-jump-to-other-file-other-window)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  (save-buffer))

(require 'clj-refactor)

(cljr-add-keybindings-with-modifier "C-s-")
(define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)

(add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))

(eval-after-load "nrepl"
  '(progn
     (define-key nrepl-mode-map (kbd "C-,") 'complete-symbol)
     (define-key nrepl-interaction-mode-map (kbd "C-,") 'complete-symbol)))

(provide 'setup-clojure-mode)
