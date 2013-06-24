(defun create-emacs-snippets (target name website major-mode)
  (interactive "sSnippets for (short name): \nsSnippets for (full name): \nsWebsite: \naMajor mode: ")
  (pa-with-new-project (format "%s-snippets" target) "emacs-snippets"
    ((cons "__target__" target)
     (cons "__name__" name)
     (cons "__website__" website)
     (cons "__major-mode__" (format "%S" major-mode))
     (cons "__year__" (format-time-string "%Y")))
    (pa-sh "carton package")))

(pa-declare-project-archetype "emacs-snippets" 'create-emacs-snippets)

(provide 'emacs-snippets-archetype)
