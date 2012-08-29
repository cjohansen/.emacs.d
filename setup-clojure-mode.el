(require 'clojure-mode)

(define-key clojure-mode-map (kbd "C-c C-j") 'clj-jump-to-other-file)
(define-key clojure-mode-map (kbd "C-c M-j") 'clj-jump-to-other-file-other-window)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(provide 'setup-clojure-mode)
