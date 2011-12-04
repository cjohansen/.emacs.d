(require 'ace-jump-mode)

(defun add-hyper-char-to-ace-jump-word-mode (c)
  (define-key global-map
    (read-kbd-macro (concat "H-" (string c)))
    `(lambda ()
       (interactive)
       (setq ace-jump-query-char ,c)
       (setq ace-jump-current-mode 'ace-jump-word-mode)
       (ace-jump-do (concat "\\b"
                            (regexp-quote (make-string 1 ,c)))))))

(loop for c from ?0 to ?9 do (add-hyper-char-to-ace-jump-word-mode c))
(loop for c from ?A to ?Z do (add-hyper-char-to-ace-jump-word-mode c))
(loop for c from ?a to ?z do (add-hyper-char-to-ace-jump-word-mode c))
(loop for c from ?Å to ?ø do (add-hyper-char-to-ace-jump-word-mode c))

(provide 'setup-ace-jump-mode)