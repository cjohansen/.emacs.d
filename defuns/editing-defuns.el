;; Basic text editing defuns

(defun new-line-below ()
  (interactive)
  (if (eolp)
      (newline)
    (end-of-line)
    (newline)
    (indent-for-tab-command)))

(defun new-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))

(defun new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (duplicate-region arg)
    (duplicate-current-line arg)))

(defun duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used. Adds the duplicated text to the kill ring."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (kill-ring-save start end)
    (goto-char end)
    (dotimes (i num)
      (insert region))))

(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (duplicate-region num (point-at-bol) (1+ (point-at-eol)))
  (goto-char (1- (point))))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines 1))
    (next-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun yank-indented ()
  (interactive)
  (let ((start (point)))
    (yank)
    (indent-region start (point))))

;; define as yank-command for delsel.el
(put 'yank-indented 'delete-selection 'yank-indented)

;; toggle quotes

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-point-forward-out-of-string ()
  (while (point-is-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  (while (point-is-in-string-p) (backward-char)))

(defun alternate-quotes-char ()
  (if (eq ?' (current-quotes-char)) ?\" ?'))

(defun toggle-quotes ()
  (interactive)
  (if (point-is-in-string-p)
      (let ((old-quotes (char-to-string (current-quotes-char)))
            (new-quotes (char-to-string (alternate-quotes-char)))
            (start (make-marker))
            (end (make-marker)))
        (save-excursion
          (move-point-forward-out-of-string)
          (backward-delete-char 1)
          (set-marker end (point))
          (insert new-quotes)
          (move-point-backward-out-of-string)
          (delete-char 1)
          (insert new-quotes)
          (set-marker start (point))
          (replace-string new-quotes (concat "\\" new-quotes) nil start end)
          (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
    (error "Point isn't in a string")))

;; kill region if active, otherwise kill backward word

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

;; Slightly more useful C-a and C-e

(defun move-end-of-line-or-next-line ()
  (interactive)
  (if (and (eolp)
           (eq last-command 'move-end-of-line-or-next-line))
      (move-end-of-line 2)
    (move-end-of-line nil)))

(defun move-start-of-line-or-prev-line ()
  (interactive)
  (if (and (bolp)
           (eq last-command 'move-start-of-line-or-prev-line))
      (move-beginning-of-line 0)
    (move-beginning-of-line nil)))

;; copy region if active
;; otherwise copy to end of current line
;;   * with prefix, copy N whole lines

(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun copy-whole-lines (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun copy-line (arg)
  "Copy to end of line, or as many lines as prefix argument"
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation"
  (interactive)
  (back-to-indentation)
  (kill-line))

(defun camelize-buffer ()
  (interactive)
  (goto-char 0)
  (condition-case nil
      (replace-next-underscore-with-camel 0)
    (error nil))
  (goto-char 0))

(defun replace-next-underscore-with-camel (arg)
  (interactive "p")
  (if (> arg 0)
      (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
  (while (not (= arg 1))
    (search-forward-regexp "\\b_[a-zA-Z]")
    (forward-char -2)
    (delete-char 1)
    (capitalize-word 1)
    (setq arg (1- arg))))

(defun snakeify-current-word ()
  (interactive)
  (er/mark-word)
  (let* ((beg (region-beginning))
         (end (region-end))
         (current-word (buffer-substring-no-properties beg end))
         (snakified (snake-case current-word)))
    (replace-string current-word snakified nil beg end)))
