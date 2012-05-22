;; Seed the random-number generator
(random t)

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; Newline after inserting closing tag in html-mode
(defadvice sgml-close-tag (after close-tag-then-newline activate)
  (newline-and-indent))

;; Add JSP expansions to html-mode
(eval-after-load "sgml-mode" '(require 'jsp-expansions))

;; Misc in misc
(setq xterm-mouse-mode t)

(provide 'misc)
