(defun js-method-function-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back ": ")))

(defun js-hoisted-function-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back "^\\s +")))

(defun snippet--function-punctuation ()
  (if (js-method-function-p)
      (insert ",")
    (unless (js-hoisted-function-p)
      (insert ";"))))

(defun snippet--function-name ()
  (if (js-hoisted-function-p) "name" ""))
