(defun er/mark-html-attribute ()
  "Mark html-attribute presumes that point is at the assignment part of attr=\"value\".
If point is inside the value-string, the quotes will be marked first anyway.
Does not support html-attributes with spaces around the equal sign or unquotes attributes atm."
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

(defun er/mark-outer-tag ()
  (interactive)
  (sgml-get-context)
  (set-mark (point))
  (sgml-skip-tag-forward 1)
  (exchange-point-and-mark))

(defun er/mark-inner-tag ()
  (interactive)
  (goto-char (aref (nth 0 (sgml-get-context)) 3))
  (set-mark (point))
  (backward-char 1)
  (sgml-skip-tag-forward 1)
  (search-backward "</")
  (exchange-point-and-mark))


(defun er/add-html-mode-expansions ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(er/mark-html-attribute
                              er/mark-inner-tag
                              er/mark-outer-tag))))

(add-hook 'html-mode-hook 'er/add-html-mode-expansions)

(provide 'html-mode-expansions)