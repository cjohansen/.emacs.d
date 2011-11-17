(defun er/mark-js-function ()
  (interactive)
  (condition-case nil
      (forward-char 8)
    (error nil))
  (word-search-backward "function")
  (set-mark (point))
  (while (not (looking-at "{"))
    (forward-char))
  (forward-list)
  (exchange-point-and-mark))

(defun er/mark-js-outer-return ()
  (interactive)
  (condition-case nil
      (forward-char 6)
    (error nil))
  (word-search-backward "return")
  (set-mark (point))
  (while (not (looking-at ";"))
    (if (looking-at "\\s(")
        (forward-list)
      (forward-char)))
  (forward-char)
  (exchange-point-and-mark))

(defun er/mark-js-inner-return ()
  (interactive)
  (condition-case nil
      (forward-char 6)
    (error nil))
  (word-search-backward "return")
  (search-forward " ")
  (set-mark (point))
  (while (not (looking-at ";"))
    (if (looking-at "\\s(")
        (forward-list)
      (forward-char)))
  (exchange-point-and-mark))

(defun er/mark-js-if ()
  (interactive)
  (condition-case nil
      (forward-char 2)
    (error nil))
  (word-search-backward "if")
  (set-mark (point))
  (while (not (looking-at "("))
    (forward-char))
  (forward-list)
  (while (not (looking-at "{"))
    (forward-char))
  (forward-list)
  (exchange-point-and-mark))

(defun er/add-js-mode-expansions ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(er/mark-js-function
                              er/mark-js-if
                              er/mark-js-inner-return
                              er/mark-js-outer-return))))

(add-hook 'js2-mode-hook 'er/add-js-mode-expansions)

(provide 'js-mode-expansions)