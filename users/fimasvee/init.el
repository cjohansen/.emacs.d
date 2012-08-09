;; Default js indentation levels
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

;; Disallow scrolling with mouse wheel
(mouse-wheel-mode nil)

;; Don't screw up key bindings in magit-mode
(add-to-list 'wrap-region-except-modes 'adventur-mode)

;; Font size
(define-key global-map (kbd "M-s +") 'zoom-in)
(define-key global-map (kbd "M-s -") 'zoom-out)

;; Experimental rebind of C-b and C-f
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "C-b") 'quick-switch-buffer) ;; toggle two most recent buffers
(global-set-key (kbd "C-f") 'duplicate-current-line-or-region) ;; duplicate line

;; Some basic elnode setup that should probably have been the default
(setq elnode-do-init nil) ;; don't start a server on port 8000 when starting emacs
(setq elnode-error-log-to-messages nil) ;; mute the crazy logging
(setq elnode-log-files-directory nil) ;; more mute

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
