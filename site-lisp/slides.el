(require 'dash)

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun focus-on-title ()
  (if (current-line-empty-p)
      (let ((curr-pos (point)))
        (next-line)
        (if (not (= curr-pos (point)))
            (focus-on-title)))))

(defun open-slide (slide)
  (let ((was-open) (find-buffer-visiting slide))
    (find-file slide)
    (setq current-slide slide)
    (if (not was-open)
        (progn
          (beginning-of-buffer)
          (focus-on-title)))))

(defun start-slideshow ()
  (interactive)
  (open-slide (buffer-file-name)))

(defun goto-slides ()
  (interactive)
  (delete-other-windows)
  (open-slide current-slide))

(defun next-slide (arg)
  (interactive "P")
  (let* ((dir (file-name-directory current-slide))
         (slides (-drop 2 (directory-files dir)))
         (idx (mod (+ (-elem-index (file-name-nondirectory current-slide) slides) (prefix-numeric-value arg))
                   (length slides))))
    (open-slide (concat dir (nth idx slides)))))

(defun prev-slide (arg)
  (interactive "P")
  (let* ((dir (file-name-directory current-slide))
         (slides (-drop 2 (directory-files dir)))
         (idx (mod (- (-elem-index (file-name-nondirectory current-slide) slides) (prefix-numeric-value arg))
                   (length slides))))
    (open-slide (concat dir (nth idx slides)))))

(global-set-key (kbd "H-s r") 'start-slideshow)
(global-set-key (kbd "H-s s") 'goto-slides)
(global-set-key (kbd "<f8>") 'goto-slides)
(global-set-key (kbd "H-s n") 'next-slide)
(global-set-key (kbd "<f9>") 'next-slide)
(global-set-key (kbd "H-s p") 'prev-slide)
(global-set-key (kbd "<f7>") 'prev-slide)

(provide 'slides)
