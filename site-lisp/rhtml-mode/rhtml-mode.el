;;;
;;; rhtml-mode.el - major mode for editing RHTML files
;;;

;; ***** BEGIN LICENSE BLOCK *****
;; Version: MPL 1.1/GPL 2.0/LGPL 2.1

;; The contents of this file are subject to the Mozilla Public License Version 
;; 1.1 (the "License"); you may not use this file except in compliance with 
;; the License. You may obtain a copy of the License at 
;; http://www.mozilla.org/MPL/

;; Software distributed under the License is distributed on an "AS IS" basis,
;; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;; for the specific language governing rights and limitations under the
;; License.

;; The Original Code is RHTML-MODE.

;; The Initial Developer of the Original Code is
;; Paul Nathan Stickney <pstickne@gmail.com>.
;; Portions created by the Initial Developer are Copyright (C) 2006
;; the Initial Developer. All Rights Reserved.

;; Contributor(s):
;;   Phil Hagelberg

;; Alternatively, the contents of this file may be used under the terms of
;; either the GNU General Public License Version 2 or later (the "GPL"), or
;; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
;; in which case the provisions of the GPL or the LGPL are applicable instead
;; of those above. If you wish to allow use of your version of this file only
;; under the terms of either the GPL or the LGPL, and not to allow others to
;; use your version of this file under the terms of the MPL, indicate your
;; decision by deleting the provisions above and replace them with the notice
;; and other provisions required by the GPL or the LGPL. If you do not delete
;; the provisions above, a recipient may use your version of this file under
;; the terms of any one of the MPL, the GPL or the LGPL.

;; ***** END LICENSE BLOCK *****


(require 'rhtml-fonts)   ;; basic fontification

;; don't require if you don't want it...
(require 'rhtml-sgml-hacks) ;; indent erb with sgml

(define-derived-mode rhtml-mode
  html-mode "RHTML"
  "Embedded Ruby Mode (RHTML)"
  (interactive)
  (abbrev-mode)
  ;; disable if you don't want it...
  (rhtml-activate-fontification))

(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode))

(define-key ruby-mode-map
  "\C-c\C-v" (lambda () (interactive) (toggle-buffer 'rails-view)))
(define-key rhtml-mode-map
  "\C-c\C-b" 'rinari-find-by-context)

(defun extract-partial (begin end partial-name)
  (interactive "r\nsName your partial: ")
  (kill-region begin end)
  (find-file (concat "_" partial-name "\\.html\\.erb"))
  (yank)
  (pop-to-buffer nil)
  (insert (concat "<%= render :partial => '" partial-name "' %>\n")))

;; PST -- uses rhtml-erb-regions which is defined in rhtml-font which
;; should be moved.
(defun rhtml-dashize (&optional mode)
  "Add or remove dashes from the end of ERb blocks. The dash tells ERb to
strip the following newline. This function will NOT add or remove dashes
from blocks that end in a # or #- sequence.

MODE controls how dashes are added or removed. If MODE is `strip' then all
ERb blocks will have the dash removed. If MODE is `add' then all blocks
will have a dash added. If MODE is `auto' or nil then ERb blocks which are
followed by a newline will have a dash added while all other blocks will
have the dash removed."
  (interactive "cDashize mode: s) strip, a) add, x) auto (default)")
  (let ((real-mode (case mode
                     ((?s strip) 'strip)
                     ((?a add) 'add))))
    (mapc (lambda (i)
            (let ((end (nth 2 i)))
              (save-excursion
                (goto-char (- end 2))
                (case (or real-mode
                          (if (eq (char-after end) ?\n)
                              'add
                            'strip))
                  (strip
                   (when (and (eq (char-before) ?-)
                              (not (eq (char-before (1- (point))) ?#)))
                     (delete-backward-char 1)))
                  (add
                   (unless (memq (char-before) '(?# ?-))
                     (insert "-")))))))
          ;; seq.
          (rhtml-erb-regions (point-min) (point-max)))))


(require 'rhtml-navigation)
(provide 'rhtml-mode)