;;; mouse-slider-mode.el --- scale numbers dragged under the mouse

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; With this minor mode enabled in a buffer, right-clicking and
;; dragging horizontally with the mouse on any number will
;; increase/decrease it. It's works like this (video is not actually
;; about this minor mode):

;;   http://youtu.be/FpxIfCHKGpQ

;; If an evaluation function is defined for the current major mode in
;; `mouse-slider-mode-eval-funcs', the local expression will also be
;; evaluated as the number is updated. For example, to add support for
;; [Skewer](https://github.com/skeeto/skewer-mode) in
;; [js2-mode](https://github.com/mooz/js2-mode),

;;   (add-to-list 'mouse-slider-mode-eval-funcs
;;                '(js2-mode . skewer-eval-defun))

;; The variable `mouse-slider-eval' enables/disables this evaluation
;; step.

;;; Code:

(require 'cl)
(require 'thingatpt)

(defvar mouse-slider-scale 1500
  "Rate at which numbers scale. Smaller means faster.")

(defvar mouse-slider-mode-eval-funcs
  `((emacs-lisp-mode . ,(apply-partially #'eval-defun nil)))
  "Alist of evaluation functions to run after scaling numbers in
various major modes.")

(defvar-local mouse-slider-eval t
  "When true, run the evaluation function listed in
`mouse-slider-mode-eval-funcs' after updating numbers.")

(defvar mouse-slider-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "<down-mouse-3>") 'mouse-slider-slide)))
  "Keymap for mouse-slider-mode.")

(define-minor-mode mouse-slider-mode
  "Scales numbers when they are right-click dragged over."
  :keymap mouse-slider-mode-map
  :lighter " MSlider")

(defvar mouse-slider-number-regexp
  "[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?"
  "Regular expression used to match numbers.")

(defun mouse-slider-number-bounds ()
  "Return the bounds of the number at point."
  (save-excursion
    (while (and (not (bobp)) (looking-at-p mouse-slider-number-regexp))
      (backward-char 1))
    (unless (bobp) (forward-char 1))
    (let ((start (point)))
      (re-search-forward mouse-slider-number-regexp)
      (cons start (point)))))

(defun* mouse-slider-replace-number (value)
  "Replace the number at point with VALUE."
  (save-excursion
    (let ((region (mouse-slider-number-bounds)))
      (delete-region (car region) (cdr region))
      (goto-char (car region))
      (insert (format "%s" value)))))

(defun mouse-slider-round (value decimals)
  "Round VALUE to DECIMALS decimal places."
  (let ((n (expt 10 decimals)))
    (/ (round (* value n)) 1.0 n)))

(defun mouse-slider-scale (base pixels)
  "Scale BASE by a drag distance of PIXELS."
  (expt base (1+ (/ pixels 1.0 mouse-slider-scale))))

(defun mouse-slider-slide (event)
  "Handle a mouse slider event by continuously updating the
number where the mouse drag began."
  (interactive "e")
  (save-excursion
    (goto-char (posn-point (second event)))
    (let ((base (thing-at-point 'number)))
      (when base
        (flet ((x (event) (car (posn-x-y (second event)))))
          (track-mouse
            (loop for movement = (read-event)
                  while (mouse-movement-p movement)
                  ;; Replace
                  for diff = (- (x movement) (x event))
                  for value = (mouse-slider-scale base diff)
                  when (not (zerop (x movement)))
                  do (mouse-slider-replace-number
                      (mouse-slider-round value 2))
                  ;; Eval
                  for f = (cdr (assoc major-mode mouse-slider-mode-eval-funcs))
                  when (and f mouse-slider-eval)
                  do (funcall f))))))))

(provide 'mouse-slider-mode)

;;; mouse-slider-mode.el ends here
