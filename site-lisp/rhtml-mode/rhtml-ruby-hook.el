;;;
;;; rhtml-ruby-hook.el - `ruby-mode' access for `rhtml-mode'
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

;; The Original Code is RUBY-MODE Hook Support for RHTML-MODE.

;; The Initial Developer of the Original Code is
;; Paul Nathan Stickney <pstickne@gmail.com>.
;; Portions created by the Initial Developer are Copyright (C) 2006
;; the Initial Developer. All Rights Reserved.

;; Contributor(s):

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


;;;
;;; Provide an API to 'hook' into Ruby-mode, or rather, provide access
;;; to a temporary `ruby-mode' buffer which can be used to apply
;;; various ruby-mode stuff, primarily indenting.
;;;

;;; History:

;; 2006SEP18
;;  - initial implementation

(require 'rhtml-erb)
(require 'ruby-mode) ; for well, `ruby-mode'

(defvar rhtml-ruby-temp-buffer-name
  "*rhtml-ruby-hook temp buffer*"
  "Buffer name to use for temporary Ruby buffer. Should begin with a * or
  space as those carry special meaning.")
  
(defun rhtml-ruby-temp-buffer ()
  "Returns the temporary ruby buffer creating it if needed."
  (or (get-buffer rhtml-ruby-temp-buffer-name)
      (let ((ruby-buffer (get-buffer-create rhtml-ruby-temp-buffer-name)))
	(with-current-buffer ruby-buffer
          (buffer-disable-undo)
          (ruby-mode))
        ruby-buffer)))

(defun rhtml-copy-to-ruby-temp (begin end)
  "Buffer to copy from should be selected.  BEGIN and END are points in the
current buffer.  All existing text in the temporary buffer is replaced."
  (let ((source-buffer (current-buffer))
        (temp-buffer (rhtml-ruby-temp-buffer)))
    (with-current-buffer temp-buffer
      (delete-region (point-min) (point-max))
      (insert-buffer-substring source-buffer begin end))))
      
(defun rhtml-ruby-indent-at (indent-pos)
  "Returns the indentation for INDENT-POS inside the temporary Ruby buffer
after updating the indenting."
  (with-current-buffer (rhtml-ruby-temp-buffer)
    (indent-region 0 indent-pos) ; force update
    (goto-char indent-pos)
    (ruby-calculate-indent)))

(defun rthml-insert-from-ruby-temp ()
  "Insert the contents of `rhtml-ruby-temp-buffer' into the current
buffer."
  (insert-from-buffer (rhtml-ruby-temp-buffer)))


(provide 'rhtml-ruby-hook)