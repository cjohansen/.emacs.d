;; Project peculiarities: FINN Oppdrag
(require 'oppdrag-mode)
(add-hook 'find-file-hook
          (lambda ()
            (when (string-match-p "oppdrag-services" (buffer-file-name))
              (oppdrag-mode))))

;; Project peculiarities: Zombie TDD
(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "zombietdd" (buffer-file-name))
              (setq js2-additional-externs '("ZOMBIE" "Faye"))
              (setq buster-default-global "ZOMBIE")
              (setq buster-add-default-global-to-iife t)
              (setq buster-use-strict t))))

;; Perspectives

(defun custom-persp/oppdrag ()
  (interactive)
  (custom-persp "oppdrag"
                (find-file "~/projects/finn-oppdrag/oppdrag-services/app-main/web/src/")))

(defun custom-persp/zombie ()
  (interactive)
  (custom-persp "zombie"
                (find-file "~/projects/zombietdd/")))

(defun custom-persp/adventur ()
  (interactive)
  (custom-persp "adventur"
                (find-file "~/projects/adventur/nettsidene/adventur_no/source/backlog.txt")))

(defun custom-persp/emacs ()
  (interactive)
  (custom-persp "emacs"
                (find-file "~/.emacs.d/init.el")))

(defun custom-persp/org ()
  (interactive)
  (custom-persp "org"
                (find-file "~/Dropbox/org/")))

(define-key persp-mode-map (kbd "C-x p o") 'custom-persp/oppdrag)
(define-key persp-mode-map (kbd "C-x p z") 'custom-persp/zombie)
(define-key persp-mode-map (kbd "C-x p a") 'custom-persp/adventur)
(define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)
(define-key persp-mode-map (kbd "C-<f6>") 'custom-persp/org)
