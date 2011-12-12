(defun rgrep-fullscreen (regexp &optional files dir confirm)
  "Open grep in full screen, saving windows."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
                                   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (read-directory-name "Base directory: "
                                          nil default-directory t))
                (confirm (equal current-prefix-arg '(4))))
           (list regexp files dir confirm))))))
  (window-configuration-to-register ?b)
  (rgrep regexp files dir confirm)
  (switch-to-buffer "*grep*")
  (delete-other-windows)
  (beginning-of-buffer))

(defun rgrep-quit-window ()
  (interactive)
  (kill-buffer)
  (jump-to-register ?b))

(add-hook 'grep-mode-hook
          (lambda ()
            (define-key grep-mode-map "q" 'rgrep-quit-window)))

(provide 'setup-rgrep)