(defun sf/skip-to-next-sexp ()
  (paredit-forward)
  (skip-syntax-forward " >"))

(defun sf/hide-region (beg end)
  (let ((o (make-overlay beg end (current-buffer) t nil)))
    (overlay-put o 'sf/hidden t)
    (overlay-put o 'invisible t)
    (overlay-put o 'display " ")
    (overlay-put o 'evaporate t)))

(defun sf/hide-mismatches (symbol)
  (let ((re (regexp-opt (list symbol) 'symbols)))
    (save-excursion
      (goto-char (point-min))
      (while (not (= (point) (point-max)))
        (let ((beg (point)))
          (if (re-search-forward re nil t)
              (progn (cljr--goto-toplevel)
                     (backward-char))
            (goto-char (point-max)))
          (unless (= beg (point))
            (sf/hide-region beg (point)))
          (paredit-forward)
          (unless (eobp)
            (forward-char)))))))

(defface sf/highlight-face
  '((t (:background "#33c")))
  "The face used to highlight symbol")

(defun sf/highlight (beg end)
  (let ((o (make-overlay beg end (current-buffer) nil t)))
    (overlay-put o 'sf/highlight t)
    (overlay-put o 'face 'sf/highlight-face)
    (overlay-put o 'evaporate t)))

(defun sf/highlight-symbol (symbol)
  (let ((l (length symbol))
        (re (regexp-opt (list symbol) 'symbols)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (sf/highlight (- (point) l) (point))))))

(defun sf/reset ()
  (remove-overlays nil nil 'sf/hidden t)
  (remove-overlays nil nil 'sf/highlight t))

(defvar sf/history nil)
(make-variable-buffer-local 'sf/history)

(defun sf/focus (symbol)
  (sf/reset)
  (sf/hide-mismatches symbol)
  (sf/highlight-symbol symbol)
  (unless (string= (car sf/history) symbol)
    (push symbol sf/history)))

(defun sf/focus-at-point ()
  (interactive)
  (sf/focus (cljr--find-symbol-at-point)))

(defun sf/back ()
  (interactive)
  (pop sf/history)
  (if (car sf/history)
      (sf/focus (car sf/history))
    (sf/reset)))

(provide 'symbol-focus)
