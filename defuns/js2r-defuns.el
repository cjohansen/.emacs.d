(defun chop-suffix (suffix s)
  "Remove string 'suffix' if it is at end of string 's'"
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defvar js2r-path-to-tests "/test/"
  "Path to tests from a root shared with sources")

(defvar js2r-path-to-sources "/lib/"
  "Path to sources from a root shared with tests")

(defvar js2r-test-suffix "-test"
  "The suffix added to test files")

(make-variable-buffer-local 'js2r-path-to-tests)
(make-variable-buffer-local 'js2r-path-to-sources)
(make-variable-buffer-local 'js2r-test-suffix)

;; Jump to source-file

(defun jump-to-source-file (arg)
  (interactive "P")
  (let ((file (guess-source-file)))
    (if (or (file-exists-p file) arg)
        (find-file file)
      (error "%s not found." file))))

(defun jump-to-source-file-other-window (arg)
  (interactive "P")
  (let ((file (guess-source-file)))
    (if (or (file-exists-p file) arg)
        (find-file-other-window file)
      (error "%s not found." file))))

(defun guess-source-file ()
  (format "%s/%s.js" (chop-suffix "/" (guess-source-folder)) (guess-source-file-name)))


(defun guess-source-file-name ()
  (chop-suffix (concat js2r-test-suffix ".js")
               (chop-suffix "Test.js"
                            (chop-suffix "_test.js"
                                         (chop-suffix "-test.js"
                                                      (file-name-nondirectory (buffer-file-name)))))))

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
  (or (test-file-that-exists "-test")
      (test-file-that-exists "_test")
      (test-file-that-exists "Test")
      (test-file-name js2r-test-suffix)))

(defun test-file-that-exists (suffix)
  (let ((file (test-file-name suffix)))
    (if (file-exists-p file) file nil)))

(defun test-file-name (suffix)
  (format "%s/%s%s.js" (chop-suffix "/" (guess-test-folder)) (test-file-name-stub) suffix))

(defun test-file-name-stub ()
  (chop-suffix ".js" (file-name-nondirectory (buffer-file-name))))

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
