(require 'wrap-region)

;; Active in all modes
(wrap-region-global-mode t)

;; Only wrap region if trigger key is given a negative prefix argument
(setq wrap-region-only-with-negative-prefix t)

;; Don't screw up key bindings in magit-mode
(add-to-list 'wrap-region-except-modes 'magit-mode)

;; Custom wrappers

(wrap-region-add-wrapper "{ value: " " }" "v" 'js2-mode)
(wrap-region-add-wrapper "$(" ")" "$" 'js2-mode)

(wrap-region-add-wrapper "<c:out value=\"" "\"/>" "o" 'html-mode)
(wrap-region-add-wrapper "<p>" "</p>" "p" 'html-mode)
(wrap-region-add-wrapper "<div>" "</div>" "d" 'html-mode)
(wrap-region-add-wrapper "<li>" "</li>" "l" 'html-mode)
(wrap-region-add-wrapper "<strong>" "</strong>" "s" 'html-mode)
(wrap-region-add-wrapper "<a href=\"\">" "</a>" "a" 'html-mode)
(wrap-region-add-wrapper "<span class=\"required\">" "</span>" "r" 'html-mode)
(wrap-region-add-wrapper "<h1>" "</h1>" "h" 'html-mode)
(wrap-region-add-wrapper "${" "}" "$" 'html-mode)

(wrap-region-add-wrapper "*" "*" "*" 'markdown-mode)

(provide 'setup-wrap-region)
