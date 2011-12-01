;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Always reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create)
  (revert-buffer))
(ad-activate 'dired-create-directory)

(provide 'setup-dired)