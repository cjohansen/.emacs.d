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

;; Use GNU ls - install with:
;;    brew install xz
;;    brew install coreutils
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

;; Set up slime-js
;; To install, see https://github.com/swank-js/swank-js/wiki/Installation

(eval-after-load "js2-mode"
  '(progn
     (require 'slime)
     (require 'slime-js)
     (require 'setup-slime-js)
     (diminish 'slime-js-minor-mode)))

(add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode 1)))
