(require 'jet)

(defun copy-edn-as-json ()
  (interactive)
  (jet-to-clipboard
   (jet--thing-at-point)
   '("--from=edn" "--to=json"))
  (deactivate-mark))

(defun copy-json-as-edn ()
  (interactive)
  (jet-to-clipboard
   (jet--thing-at-point)
   '("--from=json" "--to=edn" "--keywordize"))
  (deactivate-mark))

(global-set-key (kbd "C-c j e j") 'copy-edn-as-json)
(global-set-key (kbd "C-c j j e") 'copy-json-as-edn)

(provide 'jet-custom)
