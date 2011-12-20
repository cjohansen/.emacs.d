;;; grails-mode.el --- minor-mode that adds some Grails project management to a grails project

;; Copyright (C) 2010 Jim Morris

;; Author: Jim Morris <morris@wolfman.com>
;; Version: 0.1
;; URL: http://blog.wolfman.com

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; depends on
;; groovy-mode
;; find-cmd
;; anything
;; anything-config
;;
;;; Commentary:
;;
;; A minor mode that adds some useful commands for navigating around
;; a grails project
;;
;; In the root of the grails project (where the grails-app directory is)
;; add this to your .dir-locals.el file (v23+)
;; (groovy-mode . ((grails-mode . 1)))
;; (java-mode . ((grails-mode . 1)))
;; (html-mode . ((grails-mode . 1)))
;;
;; This will turn on grails minor mode whenever a groovy, java or gsp file is opened,
;; this presumes you have gsp files et to use html-mode adjust to whatever mode gsp files use
;;
;; or just add this to have grails mode with any file in that directory structure
;;
;; ((nil . ((grails-mode . 1))))
;;
;; The main addition is a view in anything that shows all the grails project files
;;

(require 'find-cmd)

(eval-when-compile
  (defvar anything-project-root))


;; TODO
(defun grails-jump-to-model ()
  "Jump to the domain model for the given context."
  (interactive)
  (let (var1)
    (setq var1 'some)
    (message "jump to model at point")
	)
  )

;; TODO
(defun grails-jump-to-controller ()
  "thisandthat."
  (interactive)
  (let (var1)
    (setq var1 'some)
    (message "jump to controller at point")
	)
  )

;; TODO
(defun grails-jump-to-view ()
  "Jumps the view for the controller action we are in"
  (interactive)
  (let (var1)
    (setq var1 'some)
    (message "jump to view for given action")
	)
  )

;;
;; Handle anything integration with grails project view
;;

(defvar grails-root-extra-find-args
  (find-to-string '(prune (name ".svn" ".git")))
  "Extra find args that will be AND'd to the defaults (which are
in `grails-root-file-find-process')")

(defun grails-make-displayable-name (path)
  "makes path into a displayable name. eg view(post): file, domain: file, controller: name"
  (let ((dir (file-name-directory path))
		(name (file-name-nondirectory path)))
	(let
		((type (cond 
			   ((string-match "/grails-app/views/\\([a-zA-Z0-9_]+\\)/" dir)
				(concat "view(" (match-string 1 dir) ")"))
			   ((string-match "/grails-app/controllers/.*/\\([a-zA-Z0-9_]+\\)Controller" dir) "controller")
			   ((string-match "/grails-app/domain/" dir) "domain")
			   ((string-match "/grails-app/conf/" dir) "configuration")
			   ((string-match "/grails-app/i18n/" dir) "i18n")
			   ((string-match "/grails-app/services/" dir) "services")
			   ((string-match "/grails-app/taglib/" dir) "taglib")
			   ((string-match "/grails-app/utils/" dir) "utils")
			   ((string-match "/grails-app/\\([a-zA-Z0-9_]+\\)/" dir) (match-string 1 dir))
			   (t "misc file"))))
	
	  (concat type ": " (file-name-sans-extension name)))))

(defun grails-list-project-files ()
  "Returns a list of all files found under the grails project."

  ;; find root of project
  ;; TODO should also check for grails-app directory
  (setq grails-project-root
		(locate-dominating-file default-directory "build.xml"))

  ;; get a list of all the relevant files
  (setq grails-project-files-list
		(split-string 
		 (shell-command-to-string (concat "find " grails-project-root "grails-app "
										  (find-to-string
										   `(or (name "*.groovy")
												(name "*.gsp")))))))

  ;; convert the list into cons pair of (display . filepath) where
  ;; display is a friendly name
  (setq grails-project-files-list-display
		(mapcar
		 (lambda (f)
		   (cons (grails-make-displayable-name f) f)) grails-project-files-list)))


;; anything source for showing all grails project files
(defvar anything-grails-project-files
  '((name . "Files in Grails Project")	
	(candidates . grails-project-files-list-display)
	(match anything-c-match-on-file-name)
	;(candidate-transformer nil)
    (type . file)))


(defun grails-show-project-files ()
  "Uses Anything to show all the project files"
  (interactive)
  
  (grails-list-project-files)

  (anything '(anything-grails-project-files)))


(define-minor-mode grails-mode
  "Grails Minor Mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.
     
     When Grails mode is enabled, several keys are enabled that
     will allow navigation around a typical grails project."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Grails"
  ;; The minor mode bindings.
  :keymap
  '(("\C-c/m" . grails-jump-to-model)
	("\C-c/c" . grails-jump-to-controller)
	("\C-c/v" . grails-jump-to-view)
	("\C-ca" . grails-show-project-files)
	)
  :group 'grails)

(provide 'grails-mode)
