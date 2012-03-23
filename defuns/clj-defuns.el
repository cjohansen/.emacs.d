(defvar clj-project-name "intelliadv"
  "Name of the project")

(make-variable-buffer-local 'clj-project-name)

(defun clj-source-path-snippet ()
  (concat "/src/" clj-project-name "/"))

(defun clj-test-path-snippet ()
  (concat "/test/" clj-project-name "/test/"))

(defun clj-in-source-file-p ()
  (string-match-p (clj-source-path-snippet) (buffer-file-name)))

(defun clj-in-test-file-p ()
  (string-match-p (clj-test-path-snippet) (buffer-file-name)))

(defun clj-other-file-name ()
  (when (not (or (clj-in-test-file-p)
                 (clj-in-source-file-p)))
    (error "I can't seem to find my bearings. Where are we again?"))
  (or (and (clj-in-source-file-p)
           (replace-regexp-in-string (clj-source-path-snippet)
                                     (clj-test-path-snippet)
                                     (buffer-file-name)))
      (and (clj-in-test-file-p)
           (replace-regexp-in-string (clj-test-path-snippet)
                                     (clj-source-path-snippet)
                                     (buffer-file-name)))))

(defun clj-jump-to-other-file (arg)
  (interactive "P")
  (let ((file (clj-other-file-name)))
    (if (or (file-exists-p file) arg)
        (find-file file)
      (error "%s not found." file))))

(defun clj-jump-to-other-file-other-window (arg)
  (interactive "P")
  (let ((file (clj-other-file-name)))
    (if (or (file-exists-p file) arg)
        (find-file-other-window file)
      (error "%s not found." file))))
