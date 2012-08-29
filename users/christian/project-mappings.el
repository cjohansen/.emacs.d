(defmacro project-specifics (name &rest body)
  `(progn
     (add-hook 'find-file-hook
             (lambda ()
               (when (string-match-p ,name (buffer-file-name))
                 ,@body)))
     (add-hook 'dired-after-readin-hook
             (lambda ()
               (when (string-match-p ,name (dired-current-directory))
                 ,@body)))))

;; FINN Oppdrag

(defun custom-persp/oppdrag ()
  (interactive)
  (custom-persp "oppdrag"
                (find-file "~/projects/oppdrag-services/app-main/web/src/test/javascript/todo.org")))

(define-key persp-mode-map (kbd "C-x p o") 'custom-persp/oppdrag)

(require 'oppdrag-mode)

(project-specifics "oppdrag-services"
                   (ffip-local-patterns "*.js" "*.jsp" "*.css" "*.org" "*.vm" "*jsTestDriver.conf" "*jawr.properties")
                   (oppdrag-mode))

;; Buster

(defun custom-persp/buster ()
  (interactive)
  (custom-persp "Buster"
                (find-file "~/projects/busterjs/todo.org")))

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
