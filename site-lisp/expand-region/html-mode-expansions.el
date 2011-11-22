(defun er/mark-html-attribute ()
  (interactive)
  (when (or (looking-at "\\(\\s_\\|\\sw\\)*=")
            (looking-back "="))
    (search-backward " ")
    (forward-char 1)
    (set-mark (point))
    (search-forward "=")
    (forward-char 1)
    (er--move-point-forward-out-of-string)
    (exchange-point-and-mark)))

(defun er/add-html-mode-expansions ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(er/mark-html-attribute))))

(add-hook 'html-mode-hook 'er/add-html-mode-expansions)

(provide 'html-mode-expansions)