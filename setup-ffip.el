(require 'find-file-in-project)

;; Use eproject to find project root
(setq ffip-project-root-function 'eproject-root)

;; No need to be stingy
(setq ffip-limit 4096)

;; Helper methods to create local settings

(defun ffip--create-exclude-find-options (names)
  (mapconcat (lambda (name)
               (concat "-not -regex \".*" name ".*\"")) names " "))

(defun ffip-local-excludes (&rest names)
  "Given a set of names, will exclude results with those names in the path.

Example:
(ffip-local-excludes \"target\" \"overlays\")
"
  (set (make-local-variable 'ffip-find-options)
       (ffip--create-exclude-find-options names)))

(defun ffip-local-patterns (&rest patterns)
  "An exhaustive list of file name patterns to look for.

Example:
(ffip-local-patterns \"*.js\" \"*.jsp\" \"*.css\")
"
  (set (make-local-variable 'ffip-patterns) patterns))


;; Function to create new functions that look for a specific pattern
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

;; Default excludes - override with ffip-local-excludes

(setq ffip-find-options
      (ffip--create-exclude-find-options
       '("node_modules"
         "target"
         "overlays"
         "vendor")))

(provide 'setup-ffip)
