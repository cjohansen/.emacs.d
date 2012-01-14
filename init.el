;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))

;; Set up load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" dotfiles-dir))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup extensions
(require 'setup-ido)
(require 'setup-yasnippet)
(require 'setup-dired)
(require 'setup-magit)
(require 'setup-rgrep)
(require 'setup-hippie)
(require 'setup-ace-jump-mode)
(require 'setup-perspective)
(require 'setup-shell)
;(require 'setup-autopair) -- could this be the culprit in delete-selection-mode failures?

;; Map files to modes
(require 'mode-mappings)

;; Map paths to project settings
(require 'project-mappings)

;; Hardcore mode
(require 'hardcore-mode)
;(global-hardcore-mode)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" dotfiles-dir))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
(require 'recall-position)
(require 'expand-region)
(require 'mark-more-like-this)
(require 'inline-string-rectangle)
(require 'delsel)
(require 'jump-char)
(require 'eproject)

;; Vimgolf
(load (expand-file-name "vimgolf/emacs/vimgolf.el" site-lisp-dir))

;; Setup key bindings
(require 'key-bindings)

;; Misc
(require 'appearance)
(require 'misc)
(when (equal system-type 'darwin) (require 'mac))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
