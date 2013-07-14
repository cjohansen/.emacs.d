(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq __project-name__-root-path project-directory))

(add-to-list 'load-path __project-name__-root-path)

(require '__project-name__)
(require 'espuds)
(require 'ert)

(Before
 (switch-to-buffer
  (get-buffer-create "*__project-name__*"))
 (erase-buffer)
 (fundamental-mode)
 (deactivate-mark))

(After)
