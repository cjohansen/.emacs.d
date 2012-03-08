;; FINN Oppdrag

(defun custom-persp/oppdrag ()
  (interactive)
  (custom-persp "oppdrag"
                (find-file "~/projects/finn-oppdrag/oppdrag-services/app-main/web/src/")))

(define-key persp-mode-map (kbd "C-x p o") 'custom-persp/oppdrag)

(require 'oppdrag-mode)
(add-hook 'find-file-hook
          (lambda ()
            (when (string-match-p "oppdrag-services" (buffer-file-name))
              (oppdrag-mode))))

;; Zombie TDD

(defun custom-persp/zombie ()
  (interactive)
  (custom-persp "zombie"
                (find-file "~/projects/zombietdd/todo.org")))

(define-key persp-mode-map (kbd "C-x p z") 'custom-persp/zombie)

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "zombietdd" (buffer-file-name))
              (setq js2-additional-externs '("ZOMBIE" "Faye" "EventEmitter" "when"))
              (setq buster-default-global "ZOMBIE")
              (setq buster-add-default-global-to-iife t)
              (setq buster-use-strict t)
              (setq js2r-use-strict t))))

;; culljs

(defun custom-persp/culljs ()
  (interactive)
  (custom-persp "culljs"
                (find-file "~/projects/culljs/")))

(define-key persp-mode-map (kbd "C-x p c") 'custom-persp/culljs)

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "projects/culljs" (buffer-file-name))
              (setq buster-default-global "cull")
              (setq buster-add-default-global-to-iife t)
              (setq buster-use-strict t)
              (make-variable-buffer-local 'buster-test-prefix)
              (setq buster-test-prefix "")
              (set (make-local-variable 'js2-basic-offset) 4)
              (setq js2r-use-strict t))))

;; Adventur

(defun custom-persp/adventur ()
  (interactive)
  (custom-persp "adventur"
                (find-file "~/projects/adventur/nettsidene/adventur_no/source/backlog.txt")))

(define-key persp-mode-map (kbd "C-x p a") 'custom-persp/adventur)

;; Emacs

(defun custom-persp/emacs ()
  (interactive)
  (custom-persp "emacs"
                (find-file "~/.emacs.d/init.el")))
(define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)

;; Org

(defun custom-persp/org ()
  (interactive)
  (custom-persp "org"
                (find-file "~/Dropbox/org/")))
(define-key persp-mode-map (kbd "C-<f6>") 'custom-persp/org)
