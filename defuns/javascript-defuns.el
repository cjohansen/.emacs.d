;; JavaScript specific defuns

(defun js--looking-at-object-start ()
  (and (looking-at "{")
       (not (looking-back ")[\s\n]*"))))

(defun js--goto-closest-object-start ()
  (while (not (js--looking-at-object-start))
    (if (eq (car (syntax-ppss)) 0)
        (error "Cursor is not on an object")
      (goto-char (nth 1 (syntax-ppss))))))

(defun js--ensure-newline ()
  (if (and (not (looking-at "\s*\n"))
           (not (looking-back "\n\s*")))
      (newline-and-indent)))

(defun js--ensure-just-one-space ()
  (interactive)
  (while (or (looking-at "\s*\n")
             (looking-back "\n\s*"))
    (when (looking-at "\n")
      (delete-char 1))
    (when (looking-back "\n\s")
      (backward-char)
      (delete-char -1))
    (just-one-space))
  (just-one-space))

(defmacro js--create-object-whitespace-traverser (name func)
  `(defun ,name ()
     (interactive)
     (save-excursion
       (if (not (js--looking-at-object-start))
           (js--goto-closest-object-start))
       (let ((end (make-marker)))
         (set-marker end (save-excursion
                           (forward-list)
                           (point)))
         (forward-char)
         ,func
         (while (< (point) end)
           (when (looking-at ",")
             (forward-char)
             ,func)
           (if (looking-at "\\s(")
               (forward-list)
             (forward-char)))
         (backward-char)
         ,func))))

(js--create-object-whitespace-traverser js-expand-object
                                        (js--ensure-newline))

(js--create-object-whitespace-traverser js-contract-object
                                        (js--ensure-just-one-space))

(defun js-extract-variable (name start end)
  (interactive "MVariable name: \nr")
  (let ((expression (buffer-substring start end))
        (varpos (make-marker)))
    (delete-region start end)
    (insert name)
    (set-marker varpos (point))
    (back-to-indentation)
    (insert (concat "var " name " = " expression ";\n"))
    (indent-according-to-mode)
    (goto-char varpos)))

;; Move lines up and down as list items/object attributes

(defun js--current-line-is-prefixed-with-list-item-start ()
  (save-excursion
    (back-to-indentation)
    (looking-back "\\({\\|\\[\\|,\\)\\(\s\\|\n\\)*"))) ; { or [ or , then space

(defun js--current-line-is-postfixed-with-list-item-end ()
  (save-excursion
    (end-of-line)
    (or (looking-back ",\s*") ; line ends in comma
        (looking-at "\\(\s\\|\n\\)*\\(\\]\\|}\\)")))) ; space then ] or }

(defun js--current-line-is-a-list-item ()
  (and (js--current-line-is-prefixed-with-list-item-start)
       (js--current-line-is-postfixed-with-list-item-end)))

(defun js--next-line-is-a-list-item ()
  (save-excursion
    (next-line)
    (js--current-line-is-a-list-item)))

(defun js--previous-line-is-a-list-item ()
  (save-excursion
    (previous-line)
    (js--current-line-is-a-list-item)))

(defun js--current-line-has-comma ()
  (save-excursion
    (end-of-line)
    (looking-back ",\s*")))

(defun js--previous-line-has-comma ()
  (save-excursion
    (previous-line)
    (js--current-line-has-comma)))

(defun js--move-line-down-as-list-item ()
  (move-line-down)
  (if (not (js--previous-line-has-comma))
      (save-excursion
        (end-of-line)
        (delete-char -1)
        (previous-line)
        (end-of-line)
        (insert ","))))

(defun js--move-line-up-as-list-item ()
  (move-line-up)
  (if (not (js--current-line-has-comma))
      (save-excursion
        (end-of-line)
        (insert ",")
        (next-line)
        (end-of-line)
        (delete-char -1))))

(defun js-move-line-down ()
  (interactive)
  (if (and (js--current-line-is-a-list-item)
           (js--next-line-is-a-list-item))
      (js--move-line-down-as-list-item)
    (move-line-down)))

(defun js-move-line-up ()
  (interactive)
  (if (and (js--current-line-is-a-list-item)
           (js--previous-line-is-a-list-item))
      (js--move-line-up-as-list-item)
    (move-line-up)))

(defvar js--iife-regexp "^(function (")

(defun js-inject-global-in-iife ()
  (interactive)
  (unless (use-region-p)
    (error "Mark the variable to inject first."))
  (save-excursion
    (let* ((name (buffer-substring-no-properties (region-beginning) (region-end)))
           (short (buster--global-shortcut name))
           beg end)
      (unless (search-backward-regexp js--iife-regexp)
        (error "No immediately invoked function expression found."))
      (deactivate-mark)
      (forward-char 11)
      (insert short)
      (unless (looking-at ")")
        (insert ", "))
      (search-forward "{")
      (setq beg (point))
      (backward-char)
      (forward-list)
      (forward-char)
      (setq end (point))
      (insert name)
      (unless (looking-at ")")
        (insert ", "))
      (replace-string name short t beg end))))

(provide 'javascript-defuns)
