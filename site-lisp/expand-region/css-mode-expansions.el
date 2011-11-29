(defun er/mark-css-declaration ()
  (interactive)
  (search-backward-regexp "[;{] ?")
  (forward-char)
  (set-mark (point))
  (search-forward ";" (save-excursion (end-of-line) (point)))
  (exchange-point-and-mark))

(defun er/add-css-mode-expansions ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(er/mark-css-declaration))))

(add-hook 'css-mode-hook 'er/add-css-mode-expansions)

(provide 'css-mode-expansions)