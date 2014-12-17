(require 'desktop)
;; http://www.emacswiki.org/emacs/DeskTop

(desktop-save-mode 1)
(setq history-length 250)

(add-to-list 'desktop-globals-to-save 'read-expression-history)
(add-to-list 'desktop-globals-to-save 'extended-command-history)
(add-to-list 'desktop-globals-to-save 'find-tag-history)
(add-to-list 'desktop-globals-to-save 'query-replace-history)
(add-to-list 'desktop-globals-to-save 'grep-history)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(add-to-list 'desktop-globals-to-save 'compile-history)
(add-to-list 'desktop-globals-to-save 'kill-ring)
(add-to-list 'desktop-globals-to-save 'replace-string-history)
(add-to-list 'desktop-globals-to-save 'replace-regex-history)
(add-to-list 'desktop-globals-to-save 'query-replace-regex-history)
(add-to-list 'desktop-globals-to-save 'minibuffer-history)
(add-to-list 'desktop-globals-to-save 'shell-command-history)
(add-to-list 'desktop-globals-to-save 'buffer-name-history)
(add-to-list 'desktop-globals-to-save 'find-file-history)

(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

(add-hook 'auto-save-hook 'my-desktop-save)

(provide 'setup-desktop)
