(defun s-ends-with-p (s suffix)
  "Does S end in SUFFIX?"
  (let ((pos (- (length suffix))))
    (and (>= (length s) (length suffix))
         (string= suffix (substring s pos)))))

(defun s-chop-suffix (s suffix)
  "Remove string 'suffix' if it is at end of string 's'"
  (let ((pos (- (length suffix))))
    (if (s-ends-with-p s suffix)
        (substring s 0 pos)
      s)))

(provide 's)
