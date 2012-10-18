(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/emacs-eclim/vendor"))

(require 'eclim)
(require 'eclimd)

(setq eclim-auto-save t)
(global-eclim-mode)
