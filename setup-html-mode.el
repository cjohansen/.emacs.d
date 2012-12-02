(defun skip-to-next-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (forward-char 1)
    (unless (search-forward-regexp "^\\s *$" nil t)
      (forward-char -1))))

(defun skip-to-previous-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (forward-char -1)
    (unless (search-backward-regexp "^\\s *$" nil t)
      (forward-char 1))))

(defun html-wrap-in-tag (beg end)
  (interactive "r")
  (let ((oneline? (= (line-number-at-pos beg) (line-number-at-pos end))))
    (deactivate-mark)
    (goto-char end)
    (unless oneline? (newline-and-indent))
    (insert "</div>")
    (goto-char beg)
    (insert "<div>")
    (unless oneline? (newline-and-indent))
    (indent-region beg (+ end 11))
    (goto-char (+ beg 4))))

(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map (kbd "C-<down>") 'skip-to-next-blank-line)
     (define-key html-mode-map (kbd "C-<up>") 'skip-to-previous-blank-line)
     (define-key html-mode-map (kbd "C-c C-w") 'html-wrap-in-tag)))

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

(provide 'setup-html-mode)
