;; expand sections by default
(setq magit-section-initial-visibility-alist
      '((untracked . show)
        (unstaged . show)
        (unpushed . show)
        (unpulled . show)
        (stashes . show)))

;; full screen magit-status
(defun magit-quit (&optional kill-buffer)
  "Like magit-mode-bury-buffer, but also restores the window
configuration stored by magit-status-fullscreen"
  (interactive "P")
  ;; Kill all associated Magit buffers when a double prefix arg is given.
  (when (>= (prefix-numeric-value kill-buffer) 16)
    (let ((current (current-buffer)))
      (dolist (buf (magit-mode-get-buffers))
        (unless (eq buf current)
          (kill-buffer buf)))))
  (funcall magit-bury-buffer-function kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun magit-status-fullscreen (prefix)
  (interactive "P")
  (window-configuration-to-register :magit-fullscreen)
  (magit-status)
  (unless prefix
    (delete-other-windows)))

(define-key magit-status-mode-map (kbd "q") 'magit-quit)

;; don't prompt me
(set-default 'magit-push-always-verify nil)
(set-default 'magit-revert-buffers 'silent)
(set-default 'magit-no-confirm '(stage-all-changes
                                 unstage-all-changes))

;; Real dates, please
(set-default 'magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

;; move cursor into position when entering commit message
(defun my/magit-cursor-fix ()
  (beginning-of-buffer)
  (when (looking-at "#")
    (forward-line 2)))

(add-hook 'git-commit-mode-hook 'my/magit-cursor-fix)

;; full screen vc-annotate
(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

;; move cursor into position when entering commit message

(defun my/magit-cursor-fix ()
  (beginning-of-buffer)
  (when (looking-at "#")
    (forward-line 1)))

(add-hook 'git-commit-mode-hook 'my/magit-cursor-fix)
(set-default 'magit-diff-refine-hunk t)

;; update diff-hl

(global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; use forge

(with-eval-after-load 'magit
  (require 'forge))

;; Real dates, please
(set-default 'magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

(provide 'setup-magit)
