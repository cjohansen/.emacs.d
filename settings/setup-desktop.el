(require 'desktop)
;; http://www.emacswiki.org/emacs/DeskTop

(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

(add-hook 'auto-save-hook 'my-desktop-save)

(provide 'setup-desktop)
