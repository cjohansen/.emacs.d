; mark-multiple.el
; A library that sorta lets you mark several regions at once

(defvar mm/master nil
  "The master overlay has the point. Moving point out of master clears all.")

(defvar mm/mirrors nil
  "A list of overlays that mirrors master after each change.")

(make-variable-buffer-local 'mm/master)
(make-variable-buffer-local 'mm/mirrors)

(defvar mm/keymap (make-sparse-keymap))
(define-key mm/keymap (kbd "C-g") 'mm/clear-all)
(define-key mm/keymap (kbd "<return>") 'mm/clear-all)

(defface mm/mark-face
  '((((class color) (background light)) (:background "DarkSeaGreen1"))
    (t (:background "DimGrey")))
  "The face used to highlight the marks"
  :group 'mark-multiple)

(defun mm/create-master (start end)
  "Start a new multiple mark selection by defining the master region.
Point must be within the region defined by START and END."
  (if (or (< (point) start)
          (> (point) end))
      (error "Point must be inside master region"))
  (mm/clear-all)
  (setq mm/master
        (make-overlay start end nil nil t))
  (overlay-put mm/master 'priority 100)
  (overlay-put mm/master 'face 'mm/mark-face)
  (overlay-put mm/master 'keymap mm/keymap)
  (overlay-put mm/master 'modification-hooks '(mm/on-master-modification))
  (overlay-put mm/master 'insert-in-front-hooks '(mm/on-master-modification))
  (overlay-put mm/master 'insert-behind-hooks '(mm/on-master-modification))
  (setq mm/mirrors ())
  (add-hook 'post-command-hook 'mm/post-command-handler nil t))

(defun mm/add-mirror (start end)
  "Add a region that should mirror the current master."
  (if (null mm/master)
      (error "No master defined to mirror. Start with mm/create-master."))
  (let ((mirror (make-overlay start end nil nil t)))
    (setq mm/mirrors (cons mirror mm/mirrors))
    (overlay-put mirror 'priority 100)
    (overlay-put mirror 'face 'mm/mark-face)))

(defun mm/clear-all ()
  "Remove all marks"
  (interactive)
  (when (overlayp mm/master)
    (delete-overlay mm/master)
    (dolist (mirror mm/mirrors)
      (delete-overlay mirror))
    (setq mm/master nil)
    (setq mm/mirrors ())
    (remove-hook 'post-command-hook 'mm/post-command-handler)))

(defun mm/point-is-outside-of-master ()
  "Is point outside of master?"
  (or (< (point) (overlay-start mm/master))
      (> (point) (overlay-end mm/master))))

(defun mm/post-command-handler ()
  "Clear all marks if point is outside of master"
  (if (mm/point-is-outside-of-master)
      (mm/clear-all)))

(defun mm/master-substring ()
  "Get the buffer substring that is in master"
  (buffer-substring (overlay-start mm/master) (overlay-end mm/master)))

(defun mm/on-master-modification (overlay after? beg end &optional length)
  "Update all mirrors after a change"
  (save-excursion
    (dolist (mirror mm/mirrors)
      (mm/replace-mirror-substring mirror (mm/master-substring)))))

(defun mm/replace-mirror-substring (mirror substring)
  "Replace the contents of mirror"
  (goto-char (overlay-start mirror))
  (delete-char (- (overlay-end mirror) (overlay-start mirror)))
  (insert substring))

(defun mm/last-overlay-end ()
  "Find last buffer position covered by master and mirrors"
  (let ((end (overlay-end mm/master)))
    (dolist (mirror mm/mirrors)
      (if (> (overlay-end mirror) end)
          (setq end (overlay-end mirror))))
    end))

(defun mm/mark-more-like-this (start end)
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
