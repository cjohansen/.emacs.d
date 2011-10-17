;; This directory
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Don't break lines for me, please
(setq truncate-lines 0)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

(defun comment-or-uncomment-and-indent-region ()
  "Comments or uncomments a region and re-indents"
  (interactive)
  (comment-or-uncomment-region (mark) (point))
  (indent-region (mark) (point))
  )

;; Scratch buffers
(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
	bufname)
    (while (progn
	     (setq bufname (concat "*scratch"
				   (if (= n 0) "" (int-to-string n))
				   "*"))
	     (setq n (1+ n))
	     (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (if (= n 1) (lisp-interaction-mode)) ; 1, because n was incremented
    ))

(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Auto refresh buffers
(global-auto-revert-mode)

;; Scheme
(setq scheme-program-name "/usr/bin/mzscheme")

;; Backup files
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; (setq make-backup-files nil)

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

;;
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;; Fill text always
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; For kicks
(defalias 'yes-or-no-p 'y-or-n-p)

;; Seed the random-number generator
(random t)

;; Hippie expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

;; Misc
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
(add-to-list 'load-path "~/.emacs.d/site-lisp/showoff-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/rhtml-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emms/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/recall-position")
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/eval-lines")
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit")
(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet")

;; Additional configuration
(require 'defuns)
(require 'appearance)
(require 'smooth-scrolling)
(require 'autoinsert-templates)
(require 'git-walk)
(require 'snippets)
(require 'windows)
(require 'package)
(require 'package-config)
(require 'mode-mappings)
(require 'key-bindings)
(put 'downcase-region 'disabled nil)

;; EMMS :)
(require 'emms-setup)
(emms-standard)
(emms-default-players)
(require 'emms-browser)

;; Ido
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(ido-use-filename-at-point nil)
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

;; Yahtzee, baby
(autoload 'yahtzee "yahtzee" nil t)
(autoload 'yahtzee-mode "yahtzee" nil t)
(add-to-list 'auto-mode-alist '("\\.yz\\'" . yahtzee-mode))

;; Recall position
(require 'recall-position)
(global-set-key (kbd "C-c C-s") 'toggle-buffer-pos)

;; Eval lines (Ruby only)
;; (require 'eval-lines)
;; (setq eval-ruby-interpreter "/home/christian/.rvm/rubies/ree-1.8.7-2010.02/bin/ruby")

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
