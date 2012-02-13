(defun chop-suffix (suffix s)
  "Remove string 'suffix' if it is at end of string 's'"
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

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
  (chop-suffix "Test.js"
               (chop-suffix "_test.js"
                            (chop-suffix "-test.js" (file-name-nondirectory (buffer-file-name))))))

(defun guess-source-folder ()
  (let ((test-dir (file-name-directory (buffer-file-name))))
    (if (string-match-p "/test/" test-dir)
        (let ((source-dir (source-folder-with-same-nesting test-dir)))
          (if (file-exists-p source-dir) source-dir)))))

(defun source-folder-with-same-nesting (test-dir)
  (let ((source-dir (replace-regexp-in-string ".+/test/\\(.*\\)" "lib/\\1" test-dir)))
    (concat (path-out-of-test source-dir) source-dir)))

(defun path-out-of-test (source-dir)
  (mapconcat 'identity (mapcar
                        '(lambda (word) "../")
                        (split-string source-dir "/" t)) ""))

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
      (test-file-that-exists "Test")))

(defun test-file-that-exists (suffix)
  (let ((file (test-file-name suffix)))
    (if (file-exists-p file) file nil)))

(defun test-file-name (suffix)
  (format "%s/%s%s.js" (chop-suffix "/" (guess-test-folder)) (test-file-name-stub) suffix))

(defun test-file-name-stub ()
  (chop-suffix ".js" (file-name-nondirectory (buffer-file-name))))

(defun guess-test-folder ()
  (let ((source-dir (file-name-directory (buffer-file-name))))
    (if (string-match-p "/lib/" source-dir)
        (let ((test-dir (test-folder-with-same-nesting source-dir)))
          (if (file-exists-p test-dir) test-dir)))))

(defun test-folder-with-same-nesting (source-dir)
  (let ((test-dir (replace-regexp-in-string ".+/lib/\\(.*\\)" "test/\\1" source-dir)))
    (concat (path-out-of-source test-dir) test-dir)))

(defun path-out-of-source (test-dir)
  (mapconcat 'identity (mapcar
                        '(lambda (word) "../")
                        (split-string test-dir "/" t)) ""))
