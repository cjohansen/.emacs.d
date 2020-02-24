(require 'dash)

;; change command to meta, and ignore option to use weird Norwegian keyboard
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; Norwegian mac-keyboard alt-keys)
(define-key key-translation-map (kbd "s-8") (kbd "["))
(define-key key-translation-map (kbd "s-(") (kbd "{"))
(define-key key-translation-map (kbd "s-9") (kbd "]"))
(define-key key-translation-map (kbd "s-)") (kbd "}"))
(define-key key-translation-map (kbd "s-7") (kbd "|"))
(define-key key-translation-map (kbd "s-/") (kbd "\\"))
(define-key key-translation-map (kbd "M-s-7") (kbd "M-|"))

(defun insert-backslash ()
  (interactive)
  (insert "\\"))

(global-set-key (kbd "H-7") 'insert-backslash)

(global-set-key (kbd "s-u") 'universal-argument)
(global-set-key (kbd "s--") 'negative-argument)
(--dotimes 5 (global-set-key (read-kbd-macro (format "s-%d" it)) 'digit-argument))

;; mac friendly font
(when window-system
  (setq magnars/default-font "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (setq magnars/presentation-font "-apple-Monaco-medium-normal-normal-*-21-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font magnars/default-font))

;; keybinding to toggle full screen mode
(global-set-key '[M-f10] 'toggle-frame-fullscreen)

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/usr/local/bin/aspell")

;; Open files
(defun mac-open-current-file ()
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

(global-set-key (kbd "C-c C-S-o") 'mac-open-current-file)

;; fix osx weirdness with magit avatars

(setq-default magit-revision-use-gravatar-kludge t)

(provide 'mac)
