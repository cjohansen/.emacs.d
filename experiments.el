(defun move-following-char-to-end-of-next-word ()
  "Move the char at point forward to end of next word.
Another way to think of it is that you're moving the following
word into your string or paren.  This function could use some
love to work better for that usecase."
  (interactive)
  (forward-char)
  (while (looking-at "[\s\n]")
    (transpose-chars 1))
  (while (not (looking-at "[\s\n]"))
    (transpose-chars 1))
  (backward-char))

(global-set-key (kbd "C-S-<right>") 'move-following-char-to-end-of-next-word)