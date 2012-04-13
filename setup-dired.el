(require 'dired)

;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Always reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create activate)
  (revert-buffer))

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(eval-after-load "wdired"
  '(define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files))

(provide 'setup-dired)
