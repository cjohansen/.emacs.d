(defun create-emacs-package (project-name description)
  (interactive "sProject name: \nsDescription: ")
  (pa-with-new-project project-name "emacs-package"
    ((cons "__project-name__" project-name)
     (cons "__description__" description))
    (pa-sh "carton")))

(pa-declare-project-archetype "emacs-package" 'create-emacs-package)

(provide 'emacs-package-archetype)
