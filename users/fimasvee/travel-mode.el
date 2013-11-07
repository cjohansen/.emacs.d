(require 's)

(defun travel-hot-deploy-buffer-file ()
  "If the current buffer is visiting a file, and that file is deployed
in an exploded war, re-deploy the file."
  (interactive)
  (let* ((source (buffer-file-name))
         (target (s-replace "src/main/webapp" "build/exploded" source)))
    (if (and (file-writable-p target)
             (not (string= source target)))
        (progn
          (copy-file source target t)
          (message (concat "Deployed " source " to " target)))
      (message (concat target " does not exist, file not deployed")))))

(define-minor-mode travel-mode
  "Convenience utilities for working with Finn Travel"
  nil " Travel" nil
  (if travel-mode
      (add-hook 'after-save-hook 'travel-hot-deploy-buffer-file nil t)
    (remove-hook 'after-save-hook 'travel-hot-deploy-buffer-file t)))

(provide 'travel-mode)
