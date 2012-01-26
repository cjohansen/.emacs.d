;; Seed the random-number generator
(random t)

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; Misc in misc
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t)

(provide 'misc)
