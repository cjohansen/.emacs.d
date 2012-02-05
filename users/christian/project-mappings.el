;; FINN Oppdrag

(defun custom-persp/oppdrag ()
  (interactive)
  (custom-persp "Oppdrag"
                (find-file "~/projects/finn.no/oppdrag-services/app-main/web/src/")))

(define-key persp-mode-map (kbd "C-x p o") 'custom-persp/oppdrag)

(require 'oppdrag-mode)
(add-hook 'find-file-hook
          (lambda ()
            (when (string-match-p "oppdrag-services" (buffer-file-name))
              (oppdrag-mode))))

;; Buster

(defun custom-persp/buster ()
  (interactive)
  (custom-persp "Buster"
                (find-file "~/projects/buster/todo.org")))

(define-key persp-mode-map (kbd "C-x p b") 'custom-persp/buster)

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "buster" (buffer-file-name))
              (setq js2-additional-externs '("buster"))
              (setq buster-default-global "buster")
              (setq buster-add-default-global-to-iife t)
              (setq buster-use-strict t))))

;; Gitorious

(defun custom-persp/gitorious ()
  (interactive)
  (custom-persp "Gts"
                (find-file "~/projects/gitorious/org/christian.org")))

(define-key persp-mode-map (kbd "C-x p g") 'custom-persp/gitorious)

;; Emacs

(defun custom-persp/emacs ()
  (interactive)
  (custom-persp "Emacs"
                (find-file "~/.emacs.d/init.el")))
(define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)

;; Org

(defun custom-persp/org ()
  (interactive)
  (custom-persp "org"
                (find-file "~/Dropbox/org/")))
(define-key persp-mode-map (kbd "C-<f6>") 'custom-persp/org)
