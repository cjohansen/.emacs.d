;; symbol-focus
;;
;; Hides all top-level forms that does not contain the given symbol.
;;
;; Usage: `sf/focus-at-point' to focus. `sf/back' to return.

(require 'paredit)
(require 'thingatpt)

(defun sf/skip-to-next-sexp ()
  (paredit-forward)
  (skip-syntax-forward " >"))

(defun sf/hide-region (beg end)
  (let ((o (make-overlay beg end (current-buffer) t nil)))
    (overlay-put o 'sf/hidden t)
    (overlay-put o 'invisible t)
    (overlay-put o 'display " ")
    (overlay-put o 'evaporate t)))

(defun sf/depth-at-point ()
  "Returns the depth in s-expressions, or strings, at point."
  (let ((depth (car (paredit-current-parse-state))))
    (if (paredit-in-string-p)
        (1+ depth)
      depth)))

(defun sf/goto-toplevel ()
  (paredit-backward-up (sf/depth-at-point))
  (backward-char (current-column)))

(defun sf/hide-mismatches (symbol)
  (let ((re (regexp-opt (list symbol) 'symbols)))
    (save-excursion
      (goto-char (point-min))
      (while (not (= (point) (point-max)))
        (let ((beg (point)))
          (if (re-search-forward re nil t)
              (progn (sf/goto-toplevel)
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

(defun sf/highlight-overlays ()
  (--filter (overlay-get it 'sf/highlight)
            (overlays-in (point-min) (point-max))))

(defun sf/highlight-overlay-at-point ()
  (--first (overlay-get it 'sf/highlight)
           (overlays-in (1- (point)) (1+ (point)))))

(defun sf/on-modification (overlay after? beg end &optional length)
  (when after?
    (let ((contents (buffer-substring-no-properties (overlay-start overlay)
                                                    (overlay-end overlay)))
          (inhibit-modification-hooks t))
      (save-excursion
        (--each (sf/highlight-overlays)
          (when (not (eq overlay it))
            (let ((beg (overlay-start it))
                  (end (overlay-end it)))
              (goto-char beg)
              (insert contents)
              (delete-char (- end beg)))))))))

(defun sf/highlight (beg end)
  (let ((o (make-overlay beg end (current-buffer) nil t)))
    (overlay-put o 'sf/highlight t)
    (overlay-put o 'face 'sf/highlight-face)
    (overlay-put o 'evaporate t)
    (overlay-put o 'modification-hooks '(sf/on-modification))
    (overlay-put o 'insert-in-front-hooks '(sf/on-modification))
    (overlay-put o 'insert-behind-hooks '(sf/on-modification))))

(defun sf/highlight-symbol (symbol)
  (let ((l (length symbol))
        (re (regexp-opt (list symbol) 'symbols)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (sf/highlight (- (point) l) (point))))))

(defun sf/reset ()
  (remove-overlays nil nil 'sf/hidden t)
  (remove-overlays nil nil 'sf/highlight t)
  (symbol-focus-mode 0))

(defvar sf/history nil)
(make-variable-buffer-local 'sf/history)

(defun sf/focus (symbol)
  (sf/reset)
  (sf/hide-mismatches symbol)
  (sf/highlight-symbol symbol)
  (unless (string= (car sf/history) symbol)
    (push symbol sf/history))
  (symbol-focus-mode 1)
  (recenter-top-bottom))

(defun sf/focus-at-point ()
  (interactive)
  (sf/focus (thing-at-point 'symbol)))

(defun sf/back ()
  (interactive)
  (pop sf/history)
  (if (car sf/history)
      (sf/focus (car sf/history))
    (sf/reset)))

(defun sf/next ()
  (interactive)
  (let ((next (->> (sf/highlight-overlays)
                   (--map (overlay-start it))
                   (--filter (> it (point)))
                   (-min)))
        (offset (-if-let (o (sf/highlight-overlay-at-point))
                    (- (point) (overlay-start o))
                  0)))
    (goto-char (+ next offset))))

(defun sf/prev ()
  (interactive)
  (let ((prev (->> (sf/highlight-overlays)
                   (--map (overlay-end it))
                   (--filter (< it (point)))
                   (-max)))
        (offset (-if-let (o (sf/highlight-overlay-at-point))
                    (- (overlay-end o) (point))
                  0)))
    (goto-char (- prev offset))))

(defvar symbol-focus-mode-map (make-sparse-keymap)
  "symbol-focus-mode keymap")

(define-key symbol-focus-mode-map (kbd "M-s-b") 'sf/back)
(define-key symbol-focus-mode-map (kbd "M-s-n") 'sf/next)
(define-key symbol-focus-mode-map (kbd "M-s-p") 'sf/prev)
(define-key symbol-focus-mode-map (kbd "M-s-r") 'sf/reset)

(define-minor-mode symbol-focus-mode
  "Focuses on a symbol, hides all top-level forms that does not
  include it. Easily jump between them. Change the symbol in one
  place, update all."
  nil " Focus" symbol-focus-mode-map
  (if symbol-focus-mode
      (add-hook 'before-revert-hook 'sf/reset)
    (remove-hook 'before-revert-hook 'sf/reset)))

(provide 'symbol-focus)
