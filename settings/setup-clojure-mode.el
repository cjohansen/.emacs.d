(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  (save-buffer))

(require 'clj-refactor)

(setq cljr-favor-prefix-notation nil)
(setq cljr-favor-private-functions nil)

(cljr-add-keybindings-with-modifier "C-s-")
(define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)

(require 'core-async-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (core-async-mode 1)))

(require 'symbol-focus)
(define-key clojure-mode-map (kbd "M-s-f") 'sf/focus-at-point)

(defun clj-duplicate-top-level-form ()
  (interactive)
  (save-excursion
    (cljr--goto-toplevel)
    (insert (cljr--extract-sexp) "\n")
    (cljr--just-one-blank-line)))

(define-key clojure-mode-map (kbd "M-s-d") 'clj-duplicate-top-level-form)

(add-to-list 'cljr-project-clean-functions 'cleanup-buffer)

(define-key clojure-mode-map (kbd "C->") 'cljr-thread)
(define-key clojure-mode-map (kbd "C-<") 'cljr-unwind)

(define-key clojure-mode-map (kbd "s-j") 'clj-jump-to-other-file)

(define-key clojure-mode-map (kbd "C-.") 'clj-hippie-expand-no-case-fold)

(defun clj-hippie-expand-no-case-fold ()
  (interactive)
  (let ((old-syntax (char-to-string (char-syntax ?/))))
    (modify-syntax-entry ?/ " ")
    (hippie-expand-no-case-fold)
    (modify-syntax-entry ?/ old-syntax)))

(require 'cider)

(define-key cider-repl-mode-map (kbd "<home>") nil)
(define-key cider-repl-mode-map (kbd "C-,") 'complete-symbol)
(define-key cider-mode-map (kbd "C-,") 'complete-symbol)
(define-key cider-mode-map (kbd "C-c C-q") 'nrepl-close)
(define-key cider-mode-map (kbd "C-c C-Q") 'cider-quit)

(require 'yesql-ghosts)

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

;; Pretty print results in repl
(setq cider-repl-use-pretty-printing t)

;; Don't prompt for symbols
(setq cider-prompt-for-symbol nil)

;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook #'eldoc-mode)

;; Some expectations features

(require 'clj-autotest)

(defun my-toggle-expect-focused ()
  (interactive)
  (save-excursion
    (search-backward "(expect" (cljr--point-after 'cljr--goto-toplevel))
    (forward-word)
    (if (looking-at "-focused")
        (paredit-forward-kill-word)
      (insert "-focused"))))

(defun my-remove-all-focused ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "(expect-focused" nil t)
      (delete-char -8))))

(define-key clj-refactor-map
  (cljr--key-pairs-with-modifier "C-s-" "xf") 'my-toggle-expect-focused)

(define-key clj-refactor-map
  (cljr--key-pairs-with-modifier "C-s-" "xr") 'my-remove-all-focused)


;; Cycle between () {} []

(defun live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun live-cycle-clj-coll ()
  "convert the coll at (point) from (x) -> {x} -> [x] -> (x) recur"
  (interactive)
  (let* ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "(" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal "{" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal "(" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "{" (substring (live-delete-and-extract-sexp) 1 -1) "}"))
     ((equal "{" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "[" (substring (live-delete-and-extract-sexp) 1 -1) "]"))
     ((equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "(" (substring (live-delete-and-extract-sexp) 1 -1) ")"))
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake.")))
    (goto-char original-point)))

(define-key clojure-mode-map (kbd "C-`") 'live-cycle-clj-coll)

;; Warn about missing nREPL instead of doing stupid things

(defun nrepl-warn-when-not-connected ()
  (interactive)
  (message "Oops! You're not connected to an nREPL server. Please run M-x cider or M-x cider-jack-in to connect."))

(define-key clojure-mode-map (kbd "C-M-x")   'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-x C-e") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-e") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-l") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-r") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-z") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-k") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-n") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-q") 'nrepl-warn-when-not-connected)

(setq cljr-magic-require-namespaces
      '(("io"   . "clojure.java.io")
        ("set"  . "clojure.set")
        ("str"  . "clojure.string")
        ("walk" . "clojure.walk")
        ("zip"  . "clojure.zip")
        ("time" . "clj-time.core")
        ("log"  . "taoensso.timbre")
        ("json" . "cheshire.core")))

;; Set up linting of clojure code with eastwood

;; Make sure to add [acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]
;; to your :user :dependencies in .lein/profiles.clj

(require 'flycheck-clojure)

(defun my-cider-mode-enable-flycheck ()
  (when (and (s-ends-with-p ".clj" (buffer-file-name))
             (not (s-ends-with-p "/dev/user.clj" (buffer-file-name))))
    (flycheck-mode 1)))

(add-hook 'cider-mode-hook 'my-cider-mode-enable-flycheck)

(eval-after-load 'flycheck '(add-to-list 'flycheck-checkers 'clojure-cider-eastwood))

;; Make q quit out of find-usages to previous window config

(defadvice cljr-find-usages (before setup-grep activate)
  (window-configuration-to-register ?$))

;; ------------

;; TODO: Loot more stuff from:
;;  - https://github.com/overtone/emacs-live/blob/master/packs/dev/clojure-pack/config/paredit-conf.el


(provide 'setup-clojure-mode)
