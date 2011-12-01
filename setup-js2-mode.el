(require 'js2-mode)

;; Add semicolon to end of line
(key-chord-define js2-mode-map ";;"  "\C-e;")

;; Expand and contract object
(define-key js2-mode-map (kbd "C-S-e") 'js-expand-object)
(define-key js2-mode-map (kbd "C-S-c") 'js-contract-object)

;; Line movement
(define-key js2-mode-map (kbd "<C-S-down>") 'js-move-line-down)
(define-key js2-mode-map (kbd "<C-S-up>") 'js-move-line-up)

;; Extract JavaScript variables
(define-key js2-mode-map (kbd "C-c x") 'js-extract-variable)

;; Change next underscore with a camel case
(global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)

;; js2-mode steals TAB, let's steal it back for yasnippet
(defun js2-tab-properly ()
  (interactive)
  (let ((yas/fallback-behavior 'return-nil))
    (unless (yas/expand)
      (indent-for-tab-command)
      (if (looking-back "^\s*")
          (back-to-indentation)))))

(define-key js2-mode-map (kbd "TAB") 'js2-tab-properly)

(provide 'setup-js2-mode)