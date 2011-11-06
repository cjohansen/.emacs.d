
(defun er/mark-word ()
  (interactive)
  (let ((word-regexp "\\sw"))
    (when (or (looking-at word-regexp)
              (looking-back word-regexp))
      (while (looking-at word-regexp)
        (forward-char))
      (set-mark (point))
      (while (looking-back word-regexp)
        (backward-char)))))

(defun er/mark-symbol ()
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw"))
    (when (or (looking-at symbol-regexp)
              (looking-back symbol-regexp))
      (while (looking-at symbol-regexp)
        (forward-char))
      (set-mark (point))
      (while (looking-back symbol-regexp)
        (backward-char)))))

;; Quotes

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-point-forward-out-of-string ()
  (while (point-is-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  (while (point-is-in-string-p) (backward-char)))

(defun er/mark-inside-quotes ()
 (interactive)
 (when (point-is-in-string-p)
   (move-point-backward-out-of-string)
   (forward-char)
   (set-mark (point))
   (move-point-forward-out-of-string)
   (backward-char)
   (exchange-point-and-mark)))

(defun er/mark-outside-quotes ()
 (interactive)
 (when (point-is-in-string-p)
   (move-point-backward-out-of-string)
   (set-mark (point))
   (forward-char)
   (move-point-forward-out-of-string)
   (exchange-point-and-mark)))

;; Pairs - ie [] () {} etc

(defun er/mark-inside-pairs ()
  (interactive)
  (while (not (looking-back "\\s("))
    (if (looking-back "\\s)")
        (backward-list)
      (backward-char)))
  (set-mark (point))
  (backward-char)
  (forward-list)
  (backward-char)
  (exchange-point-and-mark))

(defun er/mark-outside-pairs ()
  (interactive)
  (when (and (looking-at "\\s(")
             (region-active-p)
             (save-excursion
               (forward-list)
               (>= (mark) (point))))
        (backward-char))
  (while (not (looking-at "\\s("))
    (if (looking-back "\\s)")
        (backward-list)
      (backward-char)))
  (set-mark (point))
  (forward-list)
  (exchange-point-and-mark))

;; Expand region

(setq er/try-expand-list '(er/mark-word
                           er/mark-symbol
                           er/mark-inside-quotes
                           er/mark-outside-quotes
                           mark-paragraph
                           er/mark-inside-pairs
                           er/mark-outside-pairs))

(defun er/expand-region ()
  (interactive)
  (let ((start (point))
        (end (if (region-active-p) (mark) (point)))
        (try-list er/try-expand-list)
        (best-start 0)
        (best-end (buffer-end 1)))
    (while try-list
      (save-excursion
        (condition-case nil
            (progn
              (funcall (car try-list))
              (when (and (region-active-p)
                         (<= (point) start)
                         (>= (mark) end)
                         (> (- (mark) (point)) (- end start))
                         (>= (point) best-start))
                (setq best-start (point))
                (setq best-end (mark))
                (message "%S" (car try-list))))
          (error nil)))
      (setq try-list (cdr try-list)))
    (goto-char best-start)
    (set-mark best-end)))

(provide 'expand-region)
