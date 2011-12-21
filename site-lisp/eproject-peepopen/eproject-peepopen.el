;; eproject-peepopen.el --- Graphical file chooser for Emacs on Mac OS X.
;; Modified from peepopen.el at https://github.com/topfunky/PeepOpen-EditorSupport/blob/master/peepopen.el

;; Copyright (C) 2010 Topfunky Corporation <http://peepcode.com>

;; Licensed under the same terms as Emacs.

;; Version: 0.1.0
;; Keywords: textmate osx mac
;; Created: 8 April 2010
;; Author: Geoffrey Grosenbach <boss@topfunky.com>
;;
;; Enhancements: Josh Peek http://joshpeek.com/
;;
;; Further Modified & Renamed, 2011-05-08: Matthew Barnett (matt@wtmworldwide.com)

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; A sensible fuzzy file chooser with a beautiful Mac OS X GUI.
;; This version replaces peeopen.el's dependency on the heavy-weight
;; textmate mode with a lighter weight dependency on jrockway's eproject
;; needed only for identifying the root directory of a project
;;
;; Calls the external PeepOpen.app when you hit Meta-t (or equivalent) in eproject-mode.

;;    C-x C-o - Go to File

;;; Installation:

;; This plugin assumes that you've already loaded Jonathan Rockway's
;; eproject.el in your emacs configuration. Load this file afterward.
;;
;; Copy this file to ~/.emacs.d/vendor/eproject-peepopen.el
;;

;; You'll also need eproject.el:
;;
;;   $ cd ~/.emacs.d/vendor
;;   $ git clone https://github.com/jrockway/eproject.git

;; Require both libraries and activate eproject.
;; In most Emacs distributions, you'll do this in ~/.emacs.d/init.el
;; or your personal configuration file.
;;
;; In Aquamacs, this goes in ~/Library/Preferences/Aquamacs Emacs/Preferences.el.

;;   (add-to-list 'load-path "~/.emacs.d/vendor/eproject.el")
;;   (require 'eproject)
;;   (add-to-list 'load-path "~/.emacs.d/vendor/")
;;   (require 'eproject-peepopen)

;; For Emacs 23 or Aquamacs, use this to open files in the existing frame:
;;
;;   (setq ns-pop-up-frames nil)

;;;###autoload
(defun eproject-peepopen-goto-file-gui ()
  "Uses external GUI app to quickly jump to a file in the project."
  (interactive)
  (defun string-join (separator strings)
    "Join all STRINGS using SEPARATOR."
    (mapconcat 'identity strings separator))
  (let ((root (eproject-root)))
    (when (null root)
      (error
       (concat
        "Can't find a suitable project root")))
    (shell-command-to-string
     (format "open 'peepopen://%s?editor=%s'"
             (expand-file-name root)
             "EmacsClient"))))

(define-key eproject-mode-map (kbd "C-x C-o") #'eproject-peepopen-goto-file-gui)

(provide 'eproject-peepopen)
