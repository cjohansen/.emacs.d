(defvar single-quotes-mode-map (make-sparse-keymap)
  "single-quotes-mode keymap")

(defun single-quotes-insert ()
  (self-insert-command))

(define-key single-quotes-mode-map
  (kbd "\"") (kbd "'"))

(define-minor-mode single-quotes-mode
  "A minor mode to insert single quotes ' instead of double quotes \".
   How do you insert double quotes then? With C-q."
  nil
  "''"
  single-quotes-mode-map)

(provide 'single-quotes-mode)
