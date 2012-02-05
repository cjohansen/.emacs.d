(defun chop-suffix (suffix s)
  "Remove string 'suffix' if it is at end of string 's'"
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

;; Jump to lib-file

(defun jump-to-lib-file ()
  (interactive)
  (let ((file (guess-lib-file)))
    (if (file-exists-p file)
        (find-file file)
      (error "%s not found." file))))

(defun guess-lib-file ()
  (format "%s/%s.js" (guess-lib-folder) (guess-lib-file-name)))

(defun guess-lib-file-name ()
  (chop-suffix "Test.js"
               (chop-suffix "_test.js"
                            (chop-suffix "-test.js" (file-name-nondirectory (buffer-file-name))))))

(defun guess-lib-folder ()
  (let ((test-dir (file-name-directory (buffer-file-name))))
    (if (string-match-p "/test/" test-dir)
        (let ((lib-dir (lib-folder-with-same-nesting test-dir)))
          (if (file-exists-p lib-dir) lib-dir)))))

(defun lib-folder-with-same-nesting (test-dir)
  (let ((lib-dir (replace-regexp-in-string ".+/test/\\(.*\\)" "lib/\\1" test-dir)))
    (concat (path-out-of-test lib-dir) lib-dir)))

(defun path-out-of-test (lib-dir)
  (mapconcat 'identity (mapcar
                        '(lambda (word) "../")
                        (split-string lib-dir "/" t)) ""))

;; Jump to test-file

(defun jump-to-test-file ()
  (interactive)
  (let ((file (guess-test-file)))
    (if (file-exists-p file)
        (find-file file)
      (error "%s not found." file))))

(defun guess-test-file ()
  (or (test-file-that-exists "-test")
      (test-file-that-exists "_test")
      (test-file-that-exists "Test")))

(defun test-file-that-exists (suffix)
  (let ((file (test-file-name suffix)))
    (if (file-exists-p file) file nil)))

(defun test-file-name (suffix)
  (format "%s/%s%s.js" (guess-test-folder) (test-file-name-stub) suffix))

(defun test-file-name-stub ()
  (chop-suffix ".js" (file-name-nondirectory (buffer-file-name))))

(defun guess-test-folder ()
  (let ((lib-dir (file-name-directory (buffer-file-name))))
    (if (string-match-p "/lib/" lib-dir)
        (let ((test-dir (test-folder-with-same-nesting lib-dir)))
          (if (file-exists-p test-dir) test-dir)))))

(defun test-folder-with-same-nesting (lib-dir)
  (let ((test-dir (replace-regexp-in-string ".+/lib/\\(.*\\)" "test/\\1" lib-dir)))
    (concat (path-out-of-lib test-dir) test-dir)))

(defun path-out-of-lib (test-dir)
  (mapconcat 'identity (mapcar
                        '(lambda (word) "../")
                        (split-string test-dir "/" t)) ""))
