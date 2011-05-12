;;;
;;; rhtml-erb.el - ERB tag support for `rhtml-mode'
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

;; The Original Code is ERB Tag Support for RHTML-MODE.

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


;;; History
;; 2006SEP12 - Created

;; Brief note on conventions:
;; DELIM - refers to the things like <% and %>
;; TAG - refers to entire <%ERB%> area, -including- the delims

;; (load-file "~/.emacs.d/macro-utils.el")
;; (defmacro symbol-name-or-nil (symbol)
;;   (once-only (symbol)
;;     `(if ,symbol (symbol-name ,symbol))))
;; (put 'symbol-name-or-nil 'lisp-indent-function 1)


(defconst rhtml-erb-open-delim
  "<%"
  "ERB opening tag.
Due to implementation of `sgml-mode', this absolutely must begin with a
< and be at least two characters long to work correctly.")

(defconst rhtml-erb-close-delim
  "%>"
  "ERB ending tag.
I don't think this has any restrictions.")

(defconst rhtml-erb-open-delim-len
  (length rhtml-erb-open-delim))

(defconst rhtml-erb-close-delim-len
  (length rhtml-erb-open-delim))

(defconst rhtml-erb-delim-re
  (concat rhtml-erb-open-delim "\\|" rhtml-erb-close-delim))

(defconst rhtml-erb-tag-open-re
  (concat rhtml-erb-open-delim "\\(?:-=\\|[-=#]?\\)?"))

;; specific tags
(defconst rhtml-erb-exec-tag-open-re
  (concat rhtml-erb-open-delim "\\(?:-\\(?:[^=#]\\|$\\)\\|[^-=#]\\|$\\)")
  "<%, and who would have thought it would be so complicated?")
(defconst rhtml-erb-out-tag-open-re
  (concat rhtml-erb-open-delim "-?=")
  "<%=")
(defconst rhtml-erb-comment-tag-open-re
  (concat rhtml-erb-open-delim "-?#")
  "<%#")

(defconst rhtml-erb-tag-body-re
  "\\(?:.\\|\n\\)*?")

(defconst rhtml-erb-tag-close-re
  (concat "-?" rhtml-erb-close-delim))

(defconst rhtml-erb-tag-re
  (concat "\\(" rhtml-erb-tag-open-re "\\)"
          "\\(" rhtml-erb-tag-body-re "\\)"
          "\\(" rhtml-erb-tag-close-re "\\)"))

(defun rhtml-erb-delim-type (start-delim)
  "Return `exec', `out', `comment' or nil dependin on the type of delimeter this is."
  (flet ((match? (regex)
                 (eq (string-match regex start-delim) 0)))
    (cond ((match? rhtml-erb-exec-tag-open-re)
           'exec)
          ((match? rhtml-erb-out-tag-open-re)
           'out)
          ((match? rhtml-erb-comment-tag-open-re)
           'comment))))

(defun rhtml-erb-middle-offset (prev-line-start cur-line-start)
  "Helper method for modified `sgml-calculate-indent'.
Calculates adjustment of branches like \"else\".  PREV-LINE-START
and CUR-LINE-START should be the first non-white space on each
line, respectively."
  (save-excursion
    (+ (progn
         (goto-char cur-line-start)
         (if (rhtml-scan-for-erb-tags '(erb-middle)) sgml-basic-offset 0))
       (progn
         (goto-char prev-line-start)
         (if (rhtml-scan-for-erb-tags '(erb-middle)) (- sgml-basic-offset) 0)))))

(defconst rhtml-erb-block-open-re
  (concat "[A-Za-z_)][ ]+do[ ]+\\(?:|[A-Za-z_, ]*|\\)?[ ]*" rhtml-erb-tag-close-re))

(defconst rhtml-erb-brace-block-open-re
  (concat "[ ]+{[ ]+\\(?:|[A-Za-z_, ]*|\\)?[ ]*" rhtml-erb-tag-close-re)
  "Slightly less strictive to allow for \"hash = {\n\".")

(defmacro rhtml-erb-block-open-p ()
  "Guess if a Ruby fragment opens a block with do.
Returns `block' or `brace-block' on success."
  `(re-search-forward ,rhtml-erb-block-open-re nil t))

(defmacro rhtml-erb-brace-block-open-p ()
  "Guess if a Ruby fragment opens a brace block (with {)
Returns `block' or `brace-block' on success."
  `(re-search-forward ,rhtml-erb-brace-block-open-re nil t))

(defun rhtml-at-erb-tag-p ()
  "Returns (TAG-START . TAG-END) if at beginning of ERB tag."
  (if (looking-at rhtml-erb-tag-re)
      (cons (match-beginning 0) (match-end 0))))

(defun rhtml-skip-erb-tag ()
  "Skips over an ERB tag starting at (POINT); returns non-nil if succesful.
If the search is successful (POINT) will be advanced."
  (let ((found (rhtml-at-erb-tag-p)))
    (when found
      (goto-char (cdr found)))))

(defun rhtml-erb-tag-type-p (type)
  (memq type '(erb-open erb-middle erb-close erb-data)))

(defun rhtml-scan-for-erb-tags (tags)
  "Like `rhtml-scan-erb-tag' but will only return (ERB-TYPE . NAME)
if (memq ERB-TYPE tags)."
  (let ((start (point))
        (tag-info (rhtml-scan-erb-tag)))
    (if (memq (car tag-info) tags)
        tag-info
      ;; reset on failure
      (goto-char start)
      nil)))


(defun rhtml-scan-erb-tag ()
  "Scans an ERB tag moving (POINT) to the end and returning (ERB-TYPE . NAME) on success.
ERB-TYPE is `erb-open', `erb-data', `erb-middle', or `erb-close'.
NAME is something like \"erb-brace-block\" or \"erb-start-form-tag\" that is
used for level-matching."
  (let* ((erb-tag (rhtml-at-erb-tag-p))
         (erb-tag-end (cdr erb-tag)))
    (cond (erb-tag
           ;; Lead-in
           (looking-at rhtml-erb-tag-open-re)
           (goto-char (match-end 0))
           (skip-whitespace-forward)
           (prog1
               (save-restriction
                 (narrow-to-region (point) erb-tag-end) ;(- end 2))
                 (cond ((looking-at "if \\|unless ")
                        (cons 'erb-open "erb-multi-block"))
                       ((looking-at "for\\b\\|while ")
                        (cons 'erb-open "erb-block"))
                       ((rhtml-erb-block-open-p)
                        (cons 'erb-open "erb-block"))
                       ((rhtml-erb-brace-block-open-p)
                        (cons 'erb-open "erb-brace-block"))
                       ((looking-at "else \\|elsif")
                        (cons 'erb-middle "erb-middle"))
                       ((looking-at "end\\b")
                        (cons 'erb-close "erb-block"))
                       ((looking-at "}")
                        (cons 'erb-close "erb-brace-block"))
                       ((looking-at "start_form_tag\\b")
                        (cons 'erb-open "erb-form-tag"))
                       ((looking-at "end_form_tag\\b")
                        (cons 'erb-close "erb-form-tag"))
                       (t
                        (cons 'erb-data "erb-data"))))
             (goto-char erb-tag-end)))
        (t ;no match
         (cons nil nil)))))

;; TODO - simply by removing point parameter
(defun rhtml-erb-tag-region (&optional point)
  "If inside a ERB tag returns (START . END) of the tag, otherwise nil.
If POINT is specified it will be used instead of (POINT)."
  (if point
      (save-excursion
        (goto-char point)
        (rhtml-erb-tag-region))
    (let ((prev (save-excursion ; -> (STR . START)
                  (skip-chars-forward rhtml-erb-open-delim)
                  (when (re-search-backward rhtml-erb-delim-re nil t)
                    (cons (match-string 0) (match-beginning 0)))))
          (next (save-excursion ; -> (STR . END)
                  (skip-chars-backward rhtml-erb-open-delim)
                  (when (re-search-forward rhtml-erb-delim-re nil t)
                    (cons (match-string 0) (match-end 0))))))
      ;; limit matches to valid regions
      (when (and (string= (car prev) rhtml-erb-open-delim)
                 (string= (car next) rhtml-erb-close-delim))
        (cons (cdr prev) (cdr next))))))

(defun rhtml-erb-regions (begin end)
  "Returns a list of elements in the form (TYPE START END) where type is
`exec', `comment', `out'."
  (let* (tag-start regions last-tag-end)
    (catch 'done
      (save-excursion
        (goto-char begin)
        (while t
          (when (not (search-forward rhtml-erb-open-delim end t))
            (throw 'done regions))
          (setq tag-start (- (point) 2))
          (when (not (search-forward rhtml-erb-close-delim end t))
            (throw 'done regions))
          ;; erb tag
          (push (list
                 (case (char-after (+ tag-start 2))
                   (?= 'out) (?# 'comment) (t 'exec))
                 tag-start (point))
                regions))))))

;; PST -- what is the point? At the very least it needs a better name.
(defun rhtml-erb-regions2 (begin end)
  "Returns a list of elements in the form (TYPE START END) where type is
`exec', `comment', `out' or, for non-ERb secions, `other'."
  (let* (tag-start regions last-tag-end)
    (catch 'done
      (save-excursion
        (goto-char begin)
        (while t

          (when (not (search-forward rhtml-erb-open-delim end t))
            ;; no more erb tags
            (push (list 'other (or last-tag-end begin) end)
                  regions)
            (throw 'done regions))
          (setq tag-start (- (point) 2))

          (when (not (search-forward rhtml-erb-close-delim end t))
            (throw 'done regions))
          ;; other section
          ;; PST -- may catch partial start tag
          (when (> (point) (or last-tag-end begin))
            (push (list 'other begin (point))
                  regions))
          (setq last-tag-end (point))

          ;; erb tag
          (push (list
                 (case (char-after (+ tag-start 2))
                   (?= 'out) (?# 'comment) (t 'exec))
                 tag-start (point))
                regions))))))

(defun rhtml-union-region-containing-erb-tags (r-start r-end)
  "Returns (START . END) for a region which is an aggregate of
the region defined by R-START, R-END and any ERB tags which
start, stop, or are contained in the region."
  (let* ((unopened-tag (rhtml-erb-tag-region r-start))
         (unclosed-tag (rhtml-erb-tag-region r-end))
         (new-start (or (and unopened-tag (car unopened-tag)) r-start))
         (new-end (or (and unclosed-tag (cdr unclosed-tag)) r-end)))
    (cons new-start new-end)))

(defun rhtml-widen-to-erb-tag ()
  "Widens the buffer to the ERB tag.
If no ERB tag is found the buffer will be reset to pre-state.
The point is advanced to the beginning of the new region (even if no ERB found)."
  (let ((r-start (point-min))
        (r-end (point-max)))
    (widen)
    (let ((region (rhtml-erb-tag-region)))
      (when region
        (setq r-start (car region))
        (setq r-end (cdr region)))
      (narrow-to-region r-start r-end)
      (goto-char (point-min)))))

(defun rhtml-region-has-erb-tag-p (start end)
  "Returns non-nil if the region bounded by START and END
contains an ERB tag."
  (save-excursion
    (goto-char start)
    (re-search-forward rhtml-erb-tag-re end t)))


;; utility functions

(defun skip-whitespace-forward ()
  "Skip forward common ([ \t\r\n]) whitespace."
  (skip-chars-forward " \t\r\n"))

;;
(provide 'rhtml-erb)