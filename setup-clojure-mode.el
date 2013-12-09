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

;; Thread and unwind

(defun cljr--unwind-first ()
  (paredit-forward)
  (save-excursion
    (let* ((beg (point))
           (end (progn (paredit-forward)
                       (point)))
           (contents (buffer-substring beg end)))
      (delete-region beg end)
      (join-line -1)
      (paredit-forward-down)
      (paredit-forward)
      (insert contents)))
  (forward-char))

(defun cljr--unwind-last ()
  (paredit-forward)
  (save-excursion
    (let* ((beg (point))
           (end (progn (paredit-forward)
                       (point)))
           (contents (buffer-substring beg end)))
      (delete-region beg end)
      (join-line -1)
      (paredit-forward)
      (paredit-backward-down)
      (insert contents)))
  (forward-char))

(defun cljr-unwind ()
  (interactive)
  (forward-char 3)
  (search-backward "(->")
  (paredit-forward-down)
  (cond
   ((looking-at "->\\s ") (cljr--unwind-first))
   ((looking-at "->>\\s ") (cljr--unwind-last))))

(defun cljr--thread-first ()
  (paredit-forward-down)
  (paredit-forward)
  (let* ((beg (point))
         (end (progn (paredit-forward)
                     (point)))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (paredit-backward-up)
    (just-one-space 0)
    (insert contents)
    (newline-and-indent)))

(defun cljr--thread-last ()
  (paredit-forward 2)
  (paredit-backward-down)
  (let* ((end (point))
         (beg (progn (paredit-backward)
                     (point)))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (just-one-space 0)
    (paredit-backward-up)
    (insert contents)
    (newline-and-indent)))

(defun cljr-thread ()
  (interactive)
  (forward-char 3)
  (search-backward "(->")
  (paredit-forward-down)
  (cond
   ((looking-at "->\\s ") (cljr--thread-first))
   ((looking-at "->>\\s ") (cljr--thread-last))))

(define-key clojure-mode-map (kbd "C->") 'cljr-thread)
(define-key clojure-mode-map (kbd "C-<") 'cljr-unwind)

(provide 'setup-clojure-mode)
