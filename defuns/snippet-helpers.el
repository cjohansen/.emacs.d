(defun js-method-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back ": ")))

(defun js-function-declaration-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back "^\\s +")))

(defun snippet--function-punctuation ()
  (if (js-method-p)
      (insert ",")
    (unless (js-function-declaration-p)
      (if (looking-at "\n") (insert ";")))))

(defun snippet--function-name ()
  (if (js-function-declaration-p) "name" ""))
