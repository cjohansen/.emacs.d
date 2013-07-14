(require 'magit)

(defun create-js-buster-browser (project-name description global)
  (interactive "sProject name: \nsDescription: \nsGlobal namespace: ")
  (pa-with-new-project project-name "js-buster-browser"
    ((cons "__project-name__" project-name)
     (cons "__description__" description)
     (cons "__GLOBAL__" global))
    (magit-ignore-file ".rvmrc" nil t)
    (magit-ignore-file "todo.org" nil t)
    (pa-sh "npm link buster")
    (pa-sh "npm install")))

(pa-declare-project-archetype "js-buster-browser" 'create-js-buster-browser)

(provide 'js-buster-browser-archetype)
