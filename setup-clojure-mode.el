(require 'clojure-mode)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  (save-buffer))

(require 'clj-refactor)

(cljr-add-keybindings-with-modifier "C-s-")
(define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)

(add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))

(define-key clojure-mode-map (kbd "s-j") 'clj-jump-to-other-file)

(require 'cider)

(define-key cider-repl-mode-map (kbd "<home>") nil)
(define-key cider-repl-mode-map (kbd "C-,") 'complete-symbol)
(define-key cider-mode-map (kbd "C-,") 'complete-symbol)

;; Indent and highlight more commands
(put-clojure-indent 'match 'defun)

;; Hide nrepl buffers when switching buffers (switch to by prefixing with space)
(setq nrepl-hide-special-buffers t)

;; Enable error buffer popping also in the REPL:
(setq cider-repl-popup-stacktraces t)

;; Specify history file
(setq cider-history-file "~/.emacs.d/nrepl-history")

;; auto-select the error buffer when it's displayed
(setq cider-auto-select-error-buffer t)

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)


(provide 'setup-clojure-mode)
