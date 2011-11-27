;; Load and initialize yasnippet
(require 'yasnippet)
(yas/initialize)

;; Develop in ~/emacs.d/snippets, but also
;; include snippets that come with yasnippet
(setq yas/root-directory `(,(expand-file-name "snippets" dotfiles-dir)
                           ,(expand-file-name "yasnippet/snippets" site-lisp-dir)))

(mapc 'yas/load-directory yas/root-directory)

;; Include snippets for Buster.js
(require 'buster-snippets)

;; No dropdowns please, yas
(setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))

(provide 'setup-yasnippet)
