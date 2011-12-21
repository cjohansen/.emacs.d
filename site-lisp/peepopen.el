;; peepopen.el --- Graphical file chooser for Emacs on Mac OS X.

;; Copyright (C) 2010 Topfunky Corporation <http://peepcode.com>

;; Licensed under the same terms as Emacs.

;; Version: 0.1.0
;; Keywords: textmate osx mac
;; Created: 8 April 2010
;; Author: Geoffrey Grosenbach <boss@topfunky.com>
;;
;; Enhancements: Josh Peek http://joshpeek.com/

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; A sensible fuzzy file chooser with a beautiful Mac OS X GUI.
;;
;; This minimal enhancement to textmate-mode calls the external
;; PeepOpen.app when you hit Command-T (or equivalent).

;;    ⌘T - Go to File

;;; Installation:

;; This plugin assumes that you've already loaded Chris Wanstrath's
;; textmate.el in your emacs configuration. Load this file afterward.
;;
;; Copy this file to ~/.emacs.d/vendor/peepopen.el (or use the menu
;; item in the PeepOpen application).
;;

;; You'll also need textmate.el:
;;
;;   $ cd ~/.emacs.d/vendor
;;   $ git clone git://github.com/defunkt/textmate.el.git

;; Finally, require both libraries and activate textmate-mode.
;; In most Emacs distributions, you'll do this in ~/.emacs.d/init.el
;; or your personal configuration file.
;;
;; In Aquamacs, this goes in ~/Library/Preferences/Aquamacs Emacs/Preferences.el.

;;   (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
;;   (require 'textmate)
;;   (add-to-list 'load-path "~/.emacs.d/vendor/")
;;   (require 'peepopen)
;;   (textmate-mode)

;; For Emacs 23 or Aquamacs, use this to open files in the existing frame:
;;
;;   (setq ns-pop-up-frames nil)

;;;###autoload
(defun peepopen-goto-file-gui ()
  "Uses external GUI app to quickly jump to a file in the project."
  (interactive)
  (defun string-join (separator strings)
    "Join all STRINGS using SEPARATOR."
    (mapconcat 'identity strings separator))
  (let ((root (textmate-project-root)))
    (when (null root)
      (error
       (concat
        "Can't find a suitable project root ("
        (string-join " " *textmate-project-roots* )
        ")")))
    (shell-command-to-string
     (format "open 'peepopen://%s?editor=%s'"
             (expand-file-name root)
             (invocation-name)))))

;;;###autoload
(defun peepopen-bind-keys ()
  (cond ((featurep 'aquamacs) (peepopen-bind-aquamacs-keys))
        ((featurep 'mac-carbon) (peepopen-bind-carbon-keys))
        ((featurep 'ns) (peepopen-bind-ns-keys))))

(defun peepopen-bind-aquamacs-keys ()
  ;; Need `osx-key-mode-map' to override
  (define-key osx-key-mode-map (kbd "A-t") 'peepopen-goto-file-gui)
  (define-key *textmate-mode-map* (kbd "A-t") 'peepopen-goto-file-gui))

(defun peepopen-bind-carbon-keys ()
  (define-key *textmate-mode-map* [(meta t)] 'peepopen-goto-file-gui))

(defun peepopen-bind-ns-keys ()
  (define-key *textmate-mode-map* [(super t)] 'peepopen-goto-file-gui))

;;;###autoload
(add-hook 'textmate-mode-hook 'peepopen-bind-keys)

(provide 'peepopen)
