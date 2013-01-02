;; Default js indentation levels
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

(setq expand-region-fast-keys-enabled nil)
(setq er--show-expansion-message t)

;; Disallow scrolling with mouse wheel
(mouse-wheel-mode nil)

;; Font size
(define-key global-map (kbd "M-s +") 'zoom-in)
(define-key global-map (kbd "M-s -") 'zoom-out)

;; Experimental rebind of C-b and C-f
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "C-b") 'quick-switch-buffer) ;; toggle two most recent buffers
(global-set-key (kbd "C-f") 'duplicate-current-line-or-region) ;; duplicate line

;; Experimental super on right command key. s-x is kill-region for instance.
(setq mac-right-command-modifier 'super)

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
