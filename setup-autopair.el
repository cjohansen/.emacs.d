;; Autopair parens

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers
(setq autopair-blink nil) ;; no no no! NO BLINKING! NOOO!

(defun autopair-dont ()
  (setq autopair-dont-activate t))

;; Don't autopair lisp
(add-hook 'emacs-lisp-mode-hook 'autopair-dont)
(add-hook 'lisp-interaction-mode 'autopair-dont)

(provide 'setup-autopair)