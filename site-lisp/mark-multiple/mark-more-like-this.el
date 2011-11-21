;; Mark additional regions in buffer matching current region.
(require 'mark-multiple)

(defun mark-next-like-this (start end)
  "Find and mark the next part of the buffer matching master"
  (interactive "r")
  (let ((length (- end start)))
    (if (null mm/master)
        (mm/create-master start end))
    (save-excursion
      (goto-char (mm/last-overlay-end))
      (let ((case-fold-search nil))
        (search-forward (mm/master-substring)))
      (mm/add-mirror (- (point) length) (point)))))

(defun mark-previous-like-this (start end)
  "Find and mark the previous part of the buffer matching master"
  (interactive "r")
  (let ((length (- end start)))
    (if (null mm/master)
        (mm/create-master start end))
    (save-excursion
      (goto-char (mm/first-overlay-start))
      (let ((case-fold-search nil))
        (search-backward (mm/master-substring)))
      (mm/add-mirror (point) (+ (point) length)))))

(defun mark-more-like-this (arg)
  "Marks next part of buffer that matches masters ARG times.
Given a negative ARG it searches backwards instead."
  (interactive "pr")
  (if (not (region-active-p))
      (error "Mark a region to match first."))
  (let ((start (region-beginning))
        (end (region-end)))
    (if (> arg 0)
        (dotimes (i arg) (mark-next-like-this start end))
      (dotimes (i (- arg)) (mark-previous-like-this start end)))))

(provide 'mark-more-like-this)
