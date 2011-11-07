;; This directory
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Remove selected region if typing
(pending-delete-mode t)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Auto refresh buffers
(global-auto-revert-mode)

;; Scheme
(setq scheme-program-name "/usr/bin/mzscheme")

;; Backup files
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Interactively Do Things
(require 'ido)

(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point t
        ido-max-prospects 10))

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

;; Fill text always
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; For kicks
(defalias 'yes-or-no-p 'y-or-n-p)

;; Seed the random-number generator
(random t)

;; Hippie expand: look in buffer before filenames please
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Misc
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)
(mouse-wheel-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      delete-by-moving-to-trash t
      shift-select-mode nil
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat dotfiles-dir "oddmuse")
      xterm-mouse-mode t
      save-place-file (concat dotfiles-dir "places"))

;; Trailing white-space. Just say no.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Load path
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/rhtml-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/recall-position")
(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet")
(add-to-list 'load-path "~/.emacs.d/site-lisp/undo-tree")
(add-to-list 'load-path "~/.emacs.d/site-lisp/js2-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/oppdrag-mode")
;(add-to-list 'load-path "~/.emacs.d/site-lisp/buster-mode")

;; Additional configuration
(require 'defuns)
(require 'file-defuns)
(require 'smooth-scrolling)
(require 'git-walk)
(require 'snippets)
(require 'windows)
(require 'package)
(require 'package-config)
(require 'mode-mappings)
(require 'magit)
(require 'recall-position)
(require 'iy-go-to-char)
(require 'zoom-frm)
(require 'expand-region)
(require 'dired-details)
(require 'key-bindings)
(require 'appearance)
(when (equal system-type 'darwin)
  (require 'mac))

;; Undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save cursor position
(require 'saveplace)
(setq-default save-place t)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(ido-use-filename-at-point nil)
 '(js2-allow-rhino-new-expr-initializer nil)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-enter-indents-newline t)
 '(js2-global-externs (quote ("module" "require" "jQuery" "$" "_" "buster" "sinon" "FINN" "ZOMBIE" "assert" "refute" "testCase")))
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
