;; Load and initialize yasnippet
(require 'yasnippet)
(yas/initialize)

;; Develop in ~/emacs.d/snippets, but also
;; include snippets that come with yasnippet
(setq yas/root-directory `(,(expand-file-name "snippets" dotfiles-dir)
                           ,(expand-file-name "yasnippet/snippets" site-lisp-dir)))

(mapc 'yas/load-directory yas/root-directory)

;; Include snippets for Buster.js
(require 'buster-snippets)

;; Jump to end of snippet definition
(define-key yas/keymap (kbd "C-<return>") 'yas/exit-all-snippets)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas/snippets-at-point)))
        (position (yas/field-end (yas/snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line-or-next-line)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas/snippets-at-point)))
        (position (yas/field-start (yas/snippet-active-field snippet))))
    (if (= (point) position)
        (move-start-of-line-or-prev-line)
      (goto-char position))))

(define-key yas/keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas/keymap (kbd "C-a") 'yas/goto-start-of-active-field)

;; No dropdowns please, yas
(setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))

(provide 'setup-yasnippet)
