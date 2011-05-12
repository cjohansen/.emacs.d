;; Automatically populate new files with content
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")

;;(setq auto-insert-query nil) ;; No prompts before insertion
;;(define-auto-insert "\.html" "default.html")
(define-auto-insert "gitorious.*_test\\.rb" "ruby_test.rb")

(provide 'autoinsert-templates)
