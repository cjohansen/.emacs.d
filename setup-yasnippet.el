;; Load and initialize yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (expand-file-name "yasnippet/snippets" site-lisp-dir))

(require 'buster-snippets)

;; No dropdowns please, yas
(setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))

(provide 'setup-yasnippet)
