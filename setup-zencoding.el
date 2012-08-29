(autoload 'zencoding-mode "zencoding-mode")
(autoload 'zencoding-expand-line "zencoding-mode")
(add-hook 'sgml-mode-hook 'zencoding-mode)

(eval-after-load 'zencoding-mode
  '(progn
     (define-key zencoding-mode-keymap (kbd "C-j") nil)
     (define-key zencoding-mode-keymap (kbd "<C-return>") nil)
     (define-key zencoding-mode-keymap (kbd "C-c C-j") 'zencoding-expand-line)

     (defun zencoding-indent (text)
       "Indent the text"
       (if text
           (replace-regexp-in-string "\n" "\n  " (concat "\n" text))
         nil))

     (diminish 'zencoding-mode)

     ))

(provide 'setup-zencoding)
