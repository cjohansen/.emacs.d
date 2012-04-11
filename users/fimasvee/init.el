;; Default js indentation levels
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

;; Disallow scrolling with mouse wheel
(mouse-wheel-mode nil)

;; Don't screw up key bindings in magit-mode
(add-to-list 'wrap-region-except-modes 'adventur-mode)
