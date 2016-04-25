(require 'dash)
(require 's)

(defcustom ocljr-sort-comparator #'ocljr--string-natural-comparator
  "The comparator function to use to sort ns declaration.
Set your own if you see fit. Comparator is called with two
elements of the sub section of the ns declaration, and should
return non-nil if the first element should sort before the
second.

The following functions are also provided for use with this:
`ocljr--string-length-comparator', `ocljr--semantic-comparator',
and `ocljr--string-natural-comparator'"
  :group 'ocljr
  :type 'function)

(defun ocljr--goto-ns ()
  (goto-char (point-min))
  (if (re-search-forward clojure-namespace-name-regex nil t)
      (ocljr--goto-toplevel)
    (error "No namespace declaration found")))

(defun ocljr--search-forward-within-sexp (s &optional save-excursion)
  "Searches forward for S in the current sexp.

if SAVE-EXCURSION is T POINT does not move."
  (let ((bound (save-excursion (forward-list 1) (point))))
    (if save-excursion
        (save-excursion
          (search-forward s bound t))
      (search-forward s bound t))))

(defun ocljr--point-after (&rest actions)
  "Returns POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun ocljr--comment-line? ()
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at "\\s-*;+")))

(defun ocljr--delete-and-extract-sexp ()
  (let* ((beg (point))
         (end (ocljr--point-after 'paredit-forward))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    contents))

(defun ocljr--delete-and-extract-sexp-with-nested-sexps ()
  "Returns list of strings representing the nested sexps if there is any.
   In case there are no nested sexp the list will have only one element.
   Not recursive, does not drill down into nested sexps
   inside the first level nested sexps."
  (let* ((beg (point))
         (sexp-start beg)
         (end (progn (paredit-forward)
                     (point)))
         nested)
    (paredit-backward)
    (when (looking-at "\\[\\|(")
      (paredit-forward-down))
    (while (/= sexp-start end)
      (paredit-move-forward)
      (push (s-trim (buffer-substring sexp-start (point))) nested)
      (setq sexp-start (point)))
    (delete-region beg end)
    (nreverse (cons (concat (nth 1 nested) (car nested)) (or (nthcdr 2 nested) '())))))

(defun ocljr--extract-ns-statements (statement-type with-nested)
  (ocljr--goto-ns)
  (if (or (not (ocljr--search-forward-within-sexp (concat "(" statement-type)))
          (ocljr--comment-line?))
      '()
    (let (statements)
      (while (not (looking-at " *)"))
        (push (if with-nested
                  (ocljr--delete-and-extract-sexp-with-nested-sexps)
                (ocljr--delete-and-extract-sexp)) statements))
      statements)))

(defun ocljr--only-alpha-chars (s)
  (replace-regexp-in-string "[^[:alnum:]]" "" s))

(defun ocljr--string-natural-comparator (s1 s2)
  (string< (ocljr--only-alpha-chars s1)
           (ocljr--only-alpha-chars s2)))

(defun ocljr--string-length-comparator (s1 s2)
  (> (length s1)
     (length s2)))

(defun ocljr--extract-sexp-content (sexp)
  (replace-regexp-in-string "\\[?(?]?)?" "" sexp))

(defun ocljr--semantic-comparator (ns s1 s2)
  "Sorts used, required namespaces closer to the ns of the current buffer
   before the rest.
   When above is not applicable falls back to natural comparator."
  (let ((shared-length-s1
         (length (s-shared-start ns (ocljr--extract-sexp-content s1))))
        (shared-length-s2
         (length (s-shared-start ns (ocljr--extract-sexp-content s2)))))
    (if (/= shared-length-s1 shared-length-s2)
        (> shared-length-s1 shared-length-s2)
      (ocljr--string-natural-comparator s1 s2))))

(defun ocljr-create-comparator (comparator-fn)
  (if (eq comparator-fn 'ocljr--semantic-comparator)
      (-partial 'ocljr--semantic-comparator (clojure-find-ns))
    comparator-fn))

(defun ocljr--insert-in-ns (type)
  (ocljr--goto-ns)
  (if (ocljr--search-forward-within-sexp (concat "(" type))
      (if (looking-at " *)")
          (progn
            (search-backward "(")
            (forward-list 1)
            (forward-char -1)
            (insert " "))
        (search-backward "(")
        (forward-list 1)
        (forward-char -1)
        (newline-and-indent))
    (forward-list 1)
    (forward-char -1)
    (newline-and-indent)
    (insert "(" type " )")
    (forward-char -1)))

(defun ocljr--goto-toplevel ()
  (paredit-backward-up (cljr--depth-at-point))
  (when (looking-back "#")
    (backward-char)))

(defun ocljr-sort-ns ()
  "Sort the `ns' form according to `ocljr-sort-comparator'.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/ocljr-sort-ns"
  (interactive)
  (save-excursion
    (let ((buf-already-modified? (buffer-modified-p))
          (comparator (ocljr-create-comparator ocljr-sort-comparator)))
      (dolist (statement-type '(":require" ":use" ":import"))
        (let* ((statement        (->> (ocljr--extract-ns-statements statement-type nil)
                                      (nreverse)
                                      (-map 's-trim)))
               (sorted-statement (->> statement
                                      (-sort comparator)
                                      (-distinct))))
          (dolist (it sorted-statement)
            (ocljr--insert-in-ns statement-type)
            (insert it))
          (when (and (not buf-already-modified?)
                     (buffer-modified-p)
                     (->> (-interleave statement sorted-statement)
                          (-partition 2)
                          (--map (apply 's-equals? it))
                          (--every? (eq it t))))
            (not-modified)))))))

(provide 'old-clj-refactor-stuff)
