;; Default js indentation levels
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

;; Disallow scrolling with mouse wheel
(when window-system
  (mouse-wheel-mode -1))

;; Monday ftw
(set-variable 'calendar-week-start-day 1)

;; Font size
(define-key global-map (kbd "M-s +") 'zoom-in)
(define-key global-map (kbd "M-s -") 'zoom-out)

;; No more scrolling surprises
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-v"))

;; No graphics please o.O
(setq speedbar-use-images nil)

;; god-mode always on
(god-mode)
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'post-command-hook 'my-update-cursor)

(defun my-delete-region-and-go-to-insert-mode ()
  (delete-region (region-beginning) (region-end))
  (god-local-mode -1))

(defun my-insert-just-one-char ()
  (setq cursor-type 'bar)
  (let ((c (read-char-exclusive)))
    (insert (char-to-string c)))
  (setq cursor-type 'box))

(define-key god-local-mode-map (kbd "'")
  (λ (if (use-region-p)
         (my-delete-region-and-go-to-insert-mode)
       (my-insert-just-one-char))))

;; PHP
(autoload 'php-mode "php-mode")
(setq php-file-patterns nil)
(add-to-list 'auto-mode-alist '("[^.][^t][^p][^l]\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php$" . html-mode))
(eval-after-load "php-mode"
  '(define-key php-mode-map (kbd "C-.") nil))

;; Use GNU ls - install with:
;;    brew install xz coreutils
(setq insert-directory-program "gls")
