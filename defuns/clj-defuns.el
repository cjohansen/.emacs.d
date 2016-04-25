(require 's)

(defun clj--src-file-name-from-test (name)
  (s-with name
    (s-replace "/test/" "/src/")
    (s-replace "_test.clj" ".clj")))

(defun clj--test-file-name-from-src (name)
  (s-with name
    (s-replace "/src/" "/test/")
    (s-replace ".clj" "_test.clj")))

(defun clj--src-file-name-from-cards (name)
  (s-with name
    (s-replace "/devcards/" "/src/")
    (s-replace "_cards.clj" ".clj")))

(defun clj--cards-file-name-from-src (name)
  (s-with name
    (s-replace "/src/" "/devcards/")
    (s-replace ".clj" "_cards.clj")))

(defun clj--is-test? (name)
  (string-match-p "/test/" name))

(defun clj--is-card? (name)
  (string-match-p "/devcards/" name))

(defun clj--is-ui? (name)
  (string-match-p "/ui/" name))

(defun clj-other-file-name ()
  (let ((name (buffer-file-name)))
    (cond
     ((clj--is-test? name) (clj--src-file-name-from-test name))
     ((clj--is-card? name) (clj--src-file-name-from-cards name))
     ((clj--is-ui? name) (clj--cards-file-name-from-src name))
     (:else (clj--test-file-name-from-src name)))))

(defun clj-find-alternative-name (file)
  (cond
   ((s-ends-with? ".cljs" file)
    (s-replace ".cljs" ".cljc" file))
   ((s-ends-with? ".clj" file)
    (s-replace ".clj" ".cljc" file))
   ((s-ends-with? ".cljc" file)
    (s-replace ".cljc" ".clj" file))))

(defun clj-jump-to-other-file (arg)
  (interactive "P")
  (let* ((file (clj-other-file-name))
         (alternative-file (clj-find-alternative-name file)))
    (cond
     ((file-exists-p file) (find-file file))
     ((file-exists-p alternative-file) (find-file alternative-file))
     (arg (find-file file)
          (save-buffer))
     (t (ido-find-file-in-dir (file-name-directory file))))))

(defun clj-jump-to-other-file-other-window (arg)
  (interactive "P")
  (let ((file (clj-other-file-name)))
    (if (or (file-exists-p file) arg)
        (find-file-other-window file)
      (error "%s not found." file))))
