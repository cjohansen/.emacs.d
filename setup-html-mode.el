(defun skip-to-next-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun skip-to-previous-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

(defun html-wrap-in-tag (beg end)
  (interactive "r")
  (let ((oneline? (= (line-number-at-pos beg) (line-number-at-pos end))))
    (deactivate-mark)
    (goto-char end)
    (unless oneline? (newline-and-indent))
    (insert "</div>")
    (goto-char beg)
    (insert "<div>")
    (unless oneline? (newline-and-indent))
    (indent-region beg (+ end 11))
    (goto-char (+ beg 4))))

(defun --setup-simplezen ()
  (require 'simplezen)
  (set (make-local-variable 'yas/fallback-behavior)
       '(apply simplezen-expand-or-indent-for-tab)))

(add-hook 'sgml-mode-hook '--setup-simplezen)

(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map [remap forward-paragraph] 'skip-to-next-blank-line)
     (define-key html-mode-map [remap backward-paragraph] 'skip-to-previous-blank-line)
     (define-key html-mode-map (kbd "C-c C-w") 'html-wrap-in-tag)
     (define-key html-mode-map (kbd "/") nil) ;; no buggy matching of slashes

     (require 'tagedit)

     ;; paredit lookalikes
     (define-key html-mode-map (kbd "s-<right>") 'tagedit-forward-slurp-tag)
     (define-key html-mode-map (kbd "C-)") 'tagedit-forward-slurp-tag)
     (define-key html-mode-map (kbd "s-<left>") 'tagedit-forward-barf-tag)
     (define-key html-mode-map (kbd "C-}") 'tagedit-forward-barf-tag)
     (define-key html-mode-map (kbd "M-r") 'tagedit-raise-tag)
     (define-key html-mode-map (kbd "s-s") 'tagedit-splice-tag)

     (tagedit-add-experimental-features)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))

     ;; no paredit equivalents
     (define-key html-mode-map (kbd "s-k") 'tagedit-kill-attribute)
     (define-key html-mode-map (kbd "s-<return>") 'tagedit-toggle-multiline-tag)))

(provide 'setup-html-mode)
