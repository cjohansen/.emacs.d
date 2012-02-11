(require 'wrap-region)

;; Active in all modes
(wrap-region-global-mode t)

;; Only wrap region if trigger key is given a negative prefix argument
(setq wrap-region-only-with-negative-prefix t)

;; Custom wrappers

(wrap-region-add-wrapper "{ value: " " }" "v" 'js2-mode)

(wrap-region-add-wrapper "<c:out value=\"" "\"/>" "o" 'html-mode)
(wrap-region-add-wrapper "<p>" "</p>" "p" 'html-mode)
(wrap-region-add-wrapper "<li>" "</li>" "l" 'html-mode)

(provide 'setup-wrap-region)
