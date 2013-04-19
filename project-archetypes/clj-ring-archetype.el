(defun create-clj-ring (project-name)
  (interactive "sProject name: ")
  (pa-with-new-project project-name "clj-ring"
    ((cons "__project-name__" project-name)
     (cons "__project_name__" (s-snake-case project-name)))))

(defun pa--testing-fn ()
  (shell-command "rm -rf ~/projects/test-test" pa-out)
  (create-clj-ring "test-test")
  (find-file-other-window "~/projects/test-test"))

(pa-declare-project-archetype "clj-ring" 'create-clj-ring)

(provide 'clj-ring-archetype)
