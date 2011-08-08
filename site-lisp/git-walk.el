(defun git-walk (direction)
  (interactive)
  (shell-command (concat "git-walk " direction))
  (revert-buffer nil t))

(defun git-walk-next ()
  (interactive)
  (git-walk "next"))

(defun git-walk-prev ()
  (interactive)
  (git-walk "prev"))

(global-set-key (kbd "C-c <right>") 'git-walk-next)
(global-set-key (kbd "C-c <left>") 'git-walk-prev)

(provide 'git-walk)
