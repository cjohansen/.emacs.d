(require 'ruby-mode)

(defun ruby-end-of-block-or-parens ()
  (interactive)
  (if (looking-at "\[({[\]")
      (forward-list)
    (ruby-end-of-block)))

(defun ruby-beginning-of-block-or-parens ()
  (interactive)
  (let ((char (buffer-substring (1- (point)) (point))))
    ;; Somehow I couldn't get (looking-at "\[)}]\]") to work...
    (if (or (equal char ")")
            (equal char "}")
            (equal char "]"))
        (backward-list)
      (ruby-beginning-of-block))))

(define-key ruby-mode-map (kbd "C-M-n") 'ruby-end-of-block-or-parens)
(define-key ruby-mode-map (kbd "C-M-p") 'ruby-beginning-of-block-or-parens)


