(require 'clojure-mode)

(define-key clojure-mode-map (kbd "C-c RET jo") 'clj-jump-to-other-file)
(define-key clojure-mode-map (kbd "C-c RET oo") 'clj-jump-to-other-file-other-window)

(provide 'setup-clojure-mode)
