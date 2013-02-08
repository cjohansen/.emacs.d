;; Default js indentation levels
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

(setq expand-region-fast-keys-enabled nil)
(setq er--show-expansion-message t)

;; Disallow scrolling with mouse wheel
(mouse-wheel-mode nil)

;; Move M-s to s-s because of paredit
(global-unset-key (kbd "s-s"))
(global-set-key (kbd "s-s s--") 'snakeify-current-word)
(global-set-key (kbd "s-s e") 'sudo-edit)
(global-set-key (kbd "s-s l") 'sort-lines)
(global-set-key (kbd "s-s s") 'git-grep-fullscreen)
(global-set-key (kbd "s-s S") 'rgrep-fullscreen)
(global-set-key (kbd "s-s m") 'multi-occur)
(global-set-key (kbd "s-s M") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "s-s f") 'find-name-dired)

;; Hardcore baby
(setq too-hardcore-backspace t)
(setq too-hardcore-return t)
(require 'hardcore-mode)
(add-hook 'prog-mode-hook (lambda () (hardcore-mode 1)))
(add-hook 'js2-mode-hook (lambda () (hardcore-mode 1)))
(add-hook 'sgml-mode-hook (lambda () (hardcore-mode 1)))

;; Font size
(define-key global-map (kbd "s-s +") 'zoom-in)
(define-key global-map (kbd "s-s -") 'zoom-out)

;; Edit in Chrome

(when (require 'edit-server nil t)
  (edit-server-start))

(setq edit-server-url-major-mode-alist
      '(("github\\.com" . markdown-mode)))

;; PHP
(autoload 'php-mode "php-mode")
(setq php-file-patterns nil)
(add-to-list 'auto-mode-alist '("[^.][^t][^p][^l]\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php$" . html-mode))
(eval-after-load "php-mode"
  '(define-key php-mode-map (kbd "C-.") nil))

;; Experimental: keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-marker (mark-marker) m))
    ad-do-it))

;; Use GNU ls - install with:
;;    brew install xz coreutils
(setq insert-directory-program "gls")

;; org-mode: beginning/end of buffer on M-up/down
(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "M-<up>") 'beginning-of-buffer)
     (define-key org-mode-map (kbd "M-<down>") 'end-of-buffer)
     (define-key org-mode-map (kbd "C-S-<up>") 'org-metaup)
     (define-key org-mode-map (kbd "C-S-<down>") 'org-metadown)))

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg-staging")
