;; Load Perspective
(require 'perspective)

;; Enable perspective mode
(persp-mode t)

;; TODO: implement persp-last as before-advice on persp-switch (?)

(defmacro custom-persp (name &rest body)
  `(let ((initialize (not (gethash ,name perspectives-hash)))
         (current-perspective persp-curr))
     (persp-switch ,name)
     (persp-kill "main")
     (when initialize ,@body)
     (setq persp-last current-perspective)))

;; Jump to last perspective
(defun custom-persp-last ()
  (interactive)
  (persp-switch (persp-name persp-last)))

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
(define-key persp-mode-map (kbd "C-x p -") 'custom-persp-last)

(provide 'setup-perspective)
