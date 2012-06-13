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

;; Intelliadv

(defun custom-persp/intelliadv ()
  (interactive)
  (custom-persp "intelliadv"
                (find-file "~/projects/intelliadv/todo.org")))

(define-key persp-mode-map (kbd "C-x p i") 'custom-persp/intelliadv)

;; FINN Oppdrag

(defun custom-persp/oppdrag ()
  (interactive)
  (custom-persp "oppdrag"
                (find-file "~/Dropbox/projects/finn-oppdrag/todo.org")))

(define-key persp-mode-map (kbd "C-x p o") 'custom-persp/oppdrag)

(require 'oppdrag-mode)

(project-specifics "oppdrag-services"
                   (ffip-local-patterns "*.js" "*.jsp" "*.css" "*.org" "*.vm" "*jsTestDriver.conf" "*jawr.properties")
                   (oppdrag-mode))

;; Zombie TDD

(defun custom-persp/zombie ()
  (interactive)
  (custom-persp "zombie"
                (find-file "~/projects/zombietdd/todo.org")))

(define-key persp-mode-map (kbd "C-x p z") 'custom-persp/zombie)

(project-specifics "projects/zombietdd"
                   (ffip-local-patterns "*.js" "*.jade" "*.css" "*.json" "*.md"))

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "projects/zombietdd" (buffer-file-name))
              (fci-mode 1)
              (setq js2-additional-externs '("ZOMBIE" "Faye" "EventEmitter" "when"))
              (set (make-local-variable 'buster-default-global) "ZOMBIE")
              (set (make-local-variable 'buster-add-default-global-to-iife) t)
              (set (make-local-variable 'buster-use-strict) t)
              (set (make-local-variable 'buster-test-prefix) "")
              (set (make-local-variable 'js2r-use-strict) t))))

;; creator

(defun custom-persp/creator ()
  (interactive)
  (custom-persp "creator"
                (find-file "~/projects/creator/README.md")))

(define-key persp-mode-map (kbd "C-x p cr") 'custom-persp/creator)

(project-specifics "projects/creator"
                   (ffip-local-patterns "*.js" "*.md"))

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "projects/creator" (buffer-file-name))
              (fci-mode 1)
              (set (make-local-variable 'buster-default-global) "creator")
              (set (make-local-variable 'buster-add-default-global-to-iife) nil)
              (set (make-local-variable 'buster-use-strict) t)
              (set (make-local-variable 'buster-test-prefix) "")
              (set (make-local-variable 'js2r-use-strict) t))))


;; culljs

(defun custom-persp/culljs ()
  (interactive)
  (custom-persp "culljs"
                (find-file "~/projects/culljs/todo.org")))

(define-key persp-mode-map (kbd "C-x p cu") 'custom-persp/culljs)

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "projects/culljs" (buffer-file-name))
              (fci-mode 1)
              (setq js2-additional-externs '("cull"))
              (set (make-local-variable 'buster-default-global) "cull")
              (set (make-local-variable 'buster-add-default-global-to-iife) t)
              (set (make-local-variable 'buster-use-strict) t)
              (set (make-local-variable 'buster-test-prefix) "")
              (set (make-local-variable 'js2-basic-offset) 4)
              (set (make-local-variable 'js2r-use-strict) t))))
;; buster

(defun custom-persp/buster ()
  (interactive)
  (custom-persp "buster"
                (find-file "~/stuff/fs-watch-tree/todo.org")))

(define-key persp-mode-map (kbd "C-x p b") 'custom-persp/buster)

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "stuff/fs-watch-tree" (buffer-file-name))
              (fci-mode 1)
              (set (make-local-variable 'buster-default-global) "cull")
              (set (make-local-variable 'buster-add-default-global-to-iife) nil)
              (set (make-local-variable 'buster-use-strict) nil)
              (set (make-local-variable 'buster-test-prefix) "")
              (set (make-local-variable 'js2-basic-offset) 4)
              (set (make-local-variable 'js2r-use-strict) nil))))

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

(project-specifics ".emacs.d"
                   (ffip-local-excludes "swank")
                   (ffip-local-patterns "*.el" "*.md" "*.org"))

(define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)

;; Org

(defun custom-persp/org ()
  (interactive)
  (custom-persp "org")
  (find-file "~/Dropbox/org/"))

(define-key persp-mode-map (kbd "C-<f6>") 'custom-persp/org)
