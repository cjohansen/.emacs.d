(require 'cl)
(require 's)
(require 'dash)

(defvar js2r-path-to-tests "/test/"
  "Path to tests from a root shared with sources")

(defvar js2r-path-to-sources "/lib/"
  "Path to sources from a root shared with tests")

(defvar js2r-test-suffix "-test"
  "The suffix added to test files")

(make-variable-buffer-local 'js2r-path-to-tests)
(make-variable-buffer-local 'js2r-path-to-sources)
(make-variable-buffer-local 'js2r-test-suffix)

;; Toggle between source and test
(defun jump-between-source-and-test-files (arg)
  (interactive "P")
  (if (looks-like-test-file-name (buffer-file-name))
      (jump-to-source-file arg)
    (jump-to-test-file arg)))

(defun jump-between-source-and-test-files-other-window (arg)
  (interactive "P")
  (if (looks-like-test-file-name (buffer-file-name))
      (jump-to-source-file-other-window arg)
    (jump-to-test-file-other-window arg)))

;; Jump to source-file

(defun jump-to-source-file (arg)
  (interactive "P")
  (let ((file (guess-source-file)))
    (if (or (file-exists-p file) arg)
        (find-file file)
      (error "%s not found." file))))

(defun possible-test-file-suffixes ()
  (cons (concat js2r-test-suffix ".js")
        '("Test.js" "_test.js" "-test.js")))

(defun looks-like-test-file-name (file-name)
  (--any? (s-ends-with-p it file-name) (possible-test-file-suffixes)))

(defun jump-to-source-file-other-window (arg)
  (interactive "P")
  (let ((file (guess-source-file)))
    (if (or (file-exists-p file) arg)
        (find-file-other-window file)
      (error "%s not found." file))))

(defun guess-source-file ()
  (unless (looks-like-test-file-name (buffer-file-name))
    (error "This doesn't look like a test file."))
  (format "%s/%s.js" (s-chop-suffix "/" (guess-source-folder)) (guess-source-file-name)))

(defun guess-source-file-name ()
  (s-chop-suffixes (possible-test-file-suffixes) (file-name-nondirectory (buffer-file-name))))

(defun guess-source-folder ()
  (let ((test-dir (file-name-directory (buffer-file-name))))
    (when (not (string-match-p js2r-path-to-tests test-dir))
      (error "Unable to locate source folder. Set js2r-path-to-tests and -sources."))
    (let ((source-dir (replace-regexp-in-string
                       js2r-path-to-tests
                       js2r-path-to-sources
                       test-dir)))
      (if (file-exists-p source-dir)
          source-dir
        (error "Unable to locate source folder. Verify js2r-path-to-tests and -sources")))))


;; Jump to test-file

(defun jump-to-test-file (arg)
  (interactive "P")
  (let ((file (guess-test-file)))
    (if (or (file-exists-p file) arg)
        (find-file file)
      (error "%s not found." file))))

(defun jump-to-test-file-other-window (arg)
  (interactive "P")
  (let ((file (guess-test-file)))
    (if (or (file-exists-p file) arg)
        (find-file-other-window file)
      (error "%s not found." file))))

(defun guess-test-file ()
  (when (looks-like-test-file-name (buffer-file-name))
    (error "Looks like you're already in the test file."))
  (or (test-file-that-exists "-test")
      (test-file-that-exists "_test")
      (test-file-that-exists "Test")
      (test-file-name js2r-test-suffix)))

(defun test-file-that-exists (suffix)
  (let ((file (test-file-name suffix)))
    (if (file-exists-p file) file nil)))

(defun test-file-name (suffix)
  (format "%s/%s%s.js" (s-chop-suffix "/" (guess-test-folder)) (test-file-name-stub) suffix))

(defun test-file-name-stub ()
  (s-chop-suffix ".js" (file-name-nondirectory (buffer-file-name))))

(defun guess-test-folder ()
  (let ((source-dir (file-name-directory (buffer-file-name))))
    (when (not (string-match-p js2r-path-to-sources source-dir))
      (error "Unable to locate test folder. Set js2r-path-to-tests and -sources."))
    (let ((test-dir (replace-regexp-in-string
                     js2r-path-to-sources
                     js2r-path-to-tests
                     source-dir)))
      (if (file-exists-p test-dir)
          test-dir
        (error "Unable to locate test folder. Verify js2r-path-to-tests")))))

;; Toggle assert/refute

(defun toggle-assert-refute ()
  (interactive)
  (save-excursion
    (end-of-line)
    (re-search-backward "\\(assert\\|refute\\)")
    (if (looking-at "assert")
        (progn
          (kill-word 1)
          (insert "refute"))
      (kill-word 1)
      (insert "assert"))))

;; Mark a js2-node in right window

(defun remove-js2-mark-overlay ()
  (interactive)
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) 'mark-js2-in-right-window)
              (delete-overlay o)))
        (overlays-in (point-min) (point-max))))

(defmacro mark-js2-in-right-window (func)
  `(progn
     (kill-comment nil)
     (windmove-right)
     (remove-js2-mark-overlay)
     (let* ((node ,func)
            (beg (js2-node-abs-pos node))
            (end (js2-node-abs-end node))
            (o (make-overlay beg end nil nil t)))
       (overlay-put o 'face 'region)
       (overlay-put o 'type 'mark-js2-in-right-window)
       (windmove-left)
       (save-excursion
         (insert (format " ;; %s" (js2-node-short-name node)))))))

;;(mark-js2-in-right-window
;;  (js2-node-at-point)) ;; js2-name-node
