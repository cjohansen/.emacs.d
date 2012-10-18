(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

(defun mutt-mail-mode-hook ()
  (auto-fill-mode 1)
  (local-set-key (kbd "C-c C-c") '(lambda ()
                                    (interactive)
                                    (save-buffer)
                                    (server-edit))))

(add-hook 'mail-mode-hook 'mutt-mail-mode-hook)

(provide 'setup-mutt)
