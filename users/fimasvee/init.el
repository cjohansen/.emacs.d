;; Default js indentation levels
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

;; My taste in expand-region behavior
(setq expand-region-fast-keys-enabled nil)
(setq er--show-expansion-message t)

;; Disallow scrolling with mouse wheel
(mouse-wheel-mode -1)

;; Font size
(define-key global-map (kbd "M-s +") 'zoom-in)
(define-key global-map (kbd "M-s -") 'zoom-out)

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
