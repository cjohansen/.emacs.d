;;;
;;; rhtml-sgml-hacks.el --- add ERB contextual indenting support to sgml-mode
;;;

;;; Initial Developer: Paul Stickney <pstickne@gmail.com>, 2006

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; README

;;
;; Hacks to sgml-mode *against* Emacs 22.0.5 (2006/09/01 build)
;;  - diff'ing and editing for your version may be required!
;; Changes to original code marked with ``PST''.
;;
;; Note: `sgml-mode' indenting in Emacs 21 seems very broken.
;; Please use sgml-mode.el from Emacs 22.
;;
;; Hints to use Emacs 22 sgml-mode with Emacs 21:
;; 1) Replace all occurances of "?\s" with "?\ ". I have no idea what
;;    ?\s is supposed to mean (super?) but this lets it eval in Emacs 21.
;; 2) Comment the line containing 'defvaralias'. No reference seems to
;;    be made to the alias anyway.
;; 3) Make sure sgml-mode.el is in load-path before stock version and restart
;;    emacs.  You will have problems if the old `sgml-mode' is
;;    loaded first.
;;

;;; History

;; 2006SEP12
;;  - Created
;; 2006SEP14
;;  - enable/disable hacks use setf so they no longer require feature reloading
;; 2006SEP15
;;  - Revert back to old style unloading
;; 2006SEP19
;;  - Ruby code inside ERB blocks is now indented. See `rhtml-ruby-hook'.
;; 2006SEP22
;;  - < inside ERB tags correctly ignored.

(eval-when-compile
  (require 'cl))

(require 'rhtml-erb)
(require 'sgml-mode) ; Force load here, make sure our functions will munge in.
(require 'rhtml-ruby-hook) ; For sub-indenting


;; Crude method of hack control
;; TODO replace, see below
;;
(defun rhtml-disable-sgml-hacks ()
  "Try to return `sgml-mode' to its normal state."
  (rhtml-remove-feature 'rhtml-sgml-hacks)
  (rhtml-reload-feature 'sgml-mode))

(defun rhtml-enable-sgml-hacks ()
  "Reload `sgml-mode' hacks. Might be useful after
`rhtml-disable-sgml-hacks'."
  (rhtml-reload-feature 'sgml-mode)
  (rhtml-reload-feature 'rhtml-sgml-hacks))

(defun rhtml-remove-feature (feature)
  (setq features (delq feature features)))
(defun rhtml-reload-feature (feature)
  (rhtml-remove-feature feature)
  (require feature))


;;; Failed attempt at non feature-realoding
;;; (What is wrong?)
;; Save original functions
;;
;; (defconst rhtml-dirty-functions
;;   '(sgml-get-context
;;     sgml-calculate-indent
;;     sgml-lexical-context
;;     sgml-beginning-of-tag
;;     sgml-parse-tag-backward)
;;   "Functions to back up.")
;; (setq rhtml-hacks-backed-up nil)
;; (defconst rhtml-sgml-backup-prop 'rhtml-sgml-definition)
;; (defconst rhtml-hack-backup-prop 'rhtml-hack-definition)

;; (defun rhtml-backup-functions (prop)
;;   (dolist (fn rhtml-dirty-functions)
;;     (put fn prop (symbol-function fn))))

;; (defun rhtml-restore-functions (prop)
;;   (dolist (fn rhtml-dirty-functions)
;;     (setf fn (get fn prop))))

;; ;; Backup sgml-mode functions so we can restore them later
;; (rhtml-backup-functions rhtml-sgml-backup-prop)

;; (defun rhtml-disable-sgml-hacks ()
;;   "Restore normal functions."
;;   ;; Backup rhtml-sgml-hacks functions if not done yet.
;;   ;; Without (correctly) doing so we will accidently mix definitions!
;;   (unless rhtml-hacks-backed-up
;;     (rhtml-backup-functions rhtml-hack-backup-prop)
;;     (setq rhtml-hacks-backed-up t))
;;   (rhtml-restore-functions rhtml-sgml-backup-prop))

;; (defun rhtml-enable-sgml-hacks ()
;;   "Restore hacked functions."
;;   (rhtml-restore-functions rhtml-hack-backup-prop))

  

;; PST - handling of `erb-*'
(defun sgml-get-context (&optional until)
  "Determine the context of the current position.
By default, parse until we find a start-tag as the first thing on a line.
If UNTIL is `empty', return even if the context is empty (i.e.
we just skipped over some element and got to a beginning of line).

The context is a list of tag-info structures.  The last one is the tag
immediately enclosing the current position.

Point is assumed to be outside of any tag.  If we discover that it's
not the case, the first tag returned is the one inside which we are."
  (let ((here (point))
	(stack nil)
	(ignore nil)
	(context nil)
	tag-info)
    ;; CONTEXT keeps track of the tag-stack
    ;; STACK keeps track of the end tags we've seen (and thus the start-tags
    ;;   we'll have to ignore) when skipping over matching open..close pairs.
    ;; IGNORE is a list of tags that can be ignored because they have been
    ;;   closed implicitly.
    (skip-chars-backward " \t\n")      ; Make sure we're not at indentation.
    (while
	(and (not (eq until 'now))
	     (or stack
		 (not (if until (eq until 'empty) context))
		 (not (sgml-at-indentation-p))
		 (and context
		      (/= (point) (sgml-tag-start (car context)))
		      (sgml-unclosed-tag-p (sgml-tag-name (car context)))))
	     (setq tag-info (ignore-errors (sgml-parse-tag-backward))))

      ;; This tag may enclose things we thought were tags.  If so,
      ;; discard them.
      (while (and context
                  (> (sgml-tag-end tag-info)
                     (sgml-tag-end (car context))))
        (setq context (cdr context)))

      (cond
       ((> (sgml-tag-end tag-info) here)
	;; Oops!!  Looks like we were not outside of any tag, after all.
	(push tag-info context)
	(setq until 'now))

       ;; start-tag
       ((memq (sgml-tag-type tag-info) '(open erb-open)) ;; PST
	(cond
	 ((null stack)
	  (if (member-ignore-case (sgml-tag-name tag-info) ignore)
	      ;; There was an implicit end-tag.
	      nil
	    (push tag-info context)
	    ;; We're changing context so the tags implicitly closed inside
	    ;; the previous context aren't implicitly closed here any more.
	    ;; [ Well, actually it depends, but we don't have the info about
	    ;; when it doesn't and when it does.   --Stef ]
	    (setq ignore nil)))
	 ((eq t (compare-strings (sgml-tag-name tag-info) nil nil
                                 (car stack) nil nil t))
	  (setq stack (cdr stack)))
         ;; PST - "erb-block" closes both "erb-block" and "erb-multi-block"
         ((and (member (sgml-tag-name tag-info) '("erb-block" "erb-multi-block"))
               (string= (car stack) '"erb-block"))
          (setq stack (cdr stack)))
         ;; /PST
	 (t
	  ;; The open and close tags don't match.
	  (if (not sgml-xml-mode)
	      (unless (sgml-unclosed-tag-p (sgml-tag-name tag-info))
		(message "Unclosed tag <%s>" (sgml-tag-name tag-info))
		(let ((tmp stack))
		  ;; We could just assume that the tag is simply not closed
		  ;; but it's a bad assumption when tags *are* closed but
 		  ;; not properly nested.
		  (while (and (cdr tmp)
			      (not (eq t (compare-strings
					  (sgml-tag-name tag-info) nil nil
					  (cadr tmp) nil nil t))))
		    (setq tmp (cdr tmp)))
		  (if (cdr tmp) (setcdr tmp (cddr tmp)))))
	    (message "Unmatched tags <%s> and </%s>"
		     (sgml-tag-name tag-info) (pop stack)))))

	(if (and (null stack) (sgml-unclosed-tag-p (sgml-tag-name tag-info)))
	    ;; This is a top-level open of an implicitly closed tag, so any
	    ;; occurrence of such an open tag at the same level can be ignored
	    ;; because it's been implicitly closed.
	    (push (sgml-tag-name tag-info) ignore)))

       ;; end-tag
       ((memq (sgml-tag-type tag-info) '(close erb-close)) ;; PST
	(if (sgml-empty-tag-p (sgml-tag-name tag-info))
	    (message "Spurious </%s>: empty tag" (sgml-tag-name tag-info))
	  (push (sgml-tag-name tag-info) stack)))
       ))

    ;; return context
    context))



;; PST - added calulations for ERB tags
;; *** Bug when point at end?
(defun sgml-calculate-indent (&optional lcon)
  "Calculate the column to which this line should be indented.
LCON is the lexical context, if any."
  (unless lcon (setq lcon (sgml-lexical-context)))

  ;; Indent comment-start markers inside <!-- just like comment-end markers.
  (if (and (eq (car lcon) 'tag)
	   (looking-at "--")
	   (save-excursion (goto-char (cdr lcon)) (looking-at "<!--")))
      (setq lcon (cons 'comment (+ (cdr lcon) 2))))

  (case (car lcon)

    (string
     ;; Go back to previous non-empty line.
     (while (and (> (point) (cdr lcon))
		 (zerop (forward-line -1))
		 (looking-at "[ \t]*$")))
     (if (> (point) (cdr lcon))
	 ;; Previous line is inside the string.
	 (current-indentation)
       (goto-char (cdr lcon))
       (1+ (current-column))))

    (comment
     (let ((mark (looking-at "--")))
       ;; Go back to previous non-empty line.
       (while (and (> (point) (cdr lcon))
		   (zerop (forward-line -1))
		   (or (looking-at "[ \t]*$")
		       (if mark (not (looking-at "[ \t]*--"))))))
       (if (> (point) (cdr lcon))
	   ;; Previous line is inside the comment.
	   (skip-chars-forward " \t")
	 (goto-char (cdr lcon))
	 ;; Skip `<!' to get to the `--' with which we want to align.
	 (search-forward "--")
	 (goto-char (match-beginning 0)))
       (when (and (not mark) (looking-at "--"))
	 (forward-char 2) (skip-chars-forward " \t"))
       (current-column)))

    ;; We don't know how to indent it.  Let's be honest about it.
    (cdata nil)
    
    ;; PST - Indent Ruby inside ERB tags
    ((erb-open erb-close erb-middle erb-data)
     (let ((indent-pos (point)))
       (re-search-backward rhtml-erb-tag-open-re)
       (goto-char (match-end 0)) ; skip tag
       (let* ((content-start (point))
             (base-indent (current-column))
             ;; inside-indent-pos also accounts for sync'ing injection
             (inside-indent-pos (+ 1 base-indent (- indent-pos content-start))))
         (re-search-forward rhtml-erb-tag-close-re)
         (let ((content-end (match-beginning 0)))
           (rhtml-copy-to-ruby-temp content-start content-end)
           ;; Inject spaces into first-line (which might have had
           ;; previous contents trimmed) to keep points in sync.
           (with-current-buffer (rhtml-ruby-temp-buffer)
             (goto-char 0)
             (insert (make-string base-indent ?\ )))
           (rhtml-ruby-indent-at inside-indent-pos)))))
    ;; /PST

    (tag
     (goto-char (1+ (cdr lcon)))
     (skip-chars-forward "^ \t\n")	;Skip tag name.
     (skip-chars-forward " \t")
     (if (not (eolp))
	 (current-column)
       ;; This is the first attribute: indent.
       (goto-char (1+ (cdr lcon)))
       (+ (current-column) sgml-basic-offset)))

    (text
     (while
         (if (looking-at "</")
             (forward-sexp 1)
           ;; PST do likewise for ERB tags
           (rhtml-scan-for-erb-tags '(erb-close)))
       (skip-chars-forward " \t"))
     (let* ((here (point))
	    (unclosed (and ;; (not sgml-xml-mode)
		       (looking-at sgml-tag-name-re)
		       (member-ignore-case (match-string 1)
					   sgml-unclosed-tags)
		       (match-string 1)))
	    (context
	     ;; If possible, align on the previous non-empty text line.
	     ;; Otherwise, do a more serious parsing to find the
	     ;; tag(s) relative to which we should be indenting.
	     (if (and (not unclosed) (skip-chars-backward " \t")
		      (< (skip-chars-backward " \t\n") 0)
		      (back-to-indentation)
		      (> (point) (cdr lcon)))
		 nil
	       (goto-char here)
	       (nreverse (sgml-get-context (if unclosed nil 'empty)))))
	    (there (point)))
       ;; Ignore previous unclosed start-tag in context.
       (while (and context unclosed
		   (eq t (compare-strings
			  (sgml-tag-name (car context)) nil nil
			  unclosed nil nil t)))
	 (setq context (cdr context)))
       ;; Indent to reflect nesting.
       (cond
	;; If we were not in a text context after all, let's try again.
	((and context (> (sgml-tag-end (car context)) here))
	 (goto-char here)
	 (sgml-calculate-indent
	  (cons (if (memq (sgml-tag-type (car context)) '(comment cdata))
		    (sgml-tag-type (car context)) 'tag)
		(sgml-tag-start (car context)))))
	;; Align on the first element after the nearest open-tag, if any.
	((and context
	      (goto-char (sgml-tag-end (car context)))
	      (skip-chars-forward " \t\n")
	      (< (point) here) (sgml-at-indentation-p))
         (+ (current-column)
            (rhtml-erb-middle-offset here there))) ;; PST
	(t ;; follows another a tag
         (goto-char there)
         (+ (current-column)
            (rhtml-erb-middle-offset here there) ;; PST
            (* sgml-basic-offset (length context)))))))

    (otherwise
     (error "Unrecognized context %s" (car lcon)))

    ))



;; PST - support for `erb-*' (replaces `jsp') as well as getting name
(defun sgml-parse-tag-backward (&optional limit)
  "Parse an SGML tag backward, and return information about the tag.
Assume that parsing starts from within a textual context.
Leave point at the beginning of the tag."
  (catch 'found
    (let (tag-type tag-start tag-end name)
      (or (re-search-backward "[<>]" limit 'move)
	  (error "No tag found"))
      (when (eq (char-after) ?<)
	;; Oops!! Looks like we were not in a textual context after all!.
	;; Let's try to recover.
	(with-syntax-table sgml-tag-syntax-table
	  (let ((pos (point)))
	    (condition-case nil
		(forward-sexp)
	      (scan-error
	       ;; This < seems to be just a spurious one, let's ignore it.
	       (goto-char pos)
	       (throw 'found (sgml-parse-tag-backward limit))))
	    ;; Check it is really a tag, without any extra < or > inside.
	    (unless (sgml-tag-text-p pos (point))
	      (goto-char pos)
	      (throw 'found (sgml-parse-tag-backward limit)))
	    (forward-char -1))))
      (setq tag-end (1+ (point)))
      (cond
       ((sgml-looking-back-at "--")	; comment
	(setq tag-type 'comment
	      tag-start (search-backward "<!--" nil t)))
       ((sgml-looking-back-at "]]")	; cdata
	(setq tag-type 'cdata
	      tag-start (re-search-backward "<!\\[[A-Z]+\\[" nil t)))
       (t
	(setq tag-start
	      (with-syntax-table sgml-tag-syntax-table
		(goto-char tag-end)
		(condition-case nil
		    (backward-sexp)
		  (scan-error
		   ;; This > isn't really the end of a tag. Skip it.
		   (goto-char (1- tag-end))
		   (throw 'found (sgml-parse-tag-backward limit))))
                ;; PST, ignore <'s in ERB tags
                (when (rhtml-erb-tag-region)
                  (throw 'found (sgml-parse-tag-backward limit)))
                ;; /PST
		(point)))
	(goto-char (1+ tag-start))
	(case (char-after)
	  (?!				; declaration
	   (setq tag-type 'decl))
	  (??				; processing-instruction
	   (setq tag-type 'pi))
	  (?/				; close-tag
	   (forward-char 1)
	   (setq tag-type 'close
		 name (sgml-parse-tag-name)))
          ;; PST - ERB
          ;; TODO does not work with defvar delim setup.
	  (?%
           (backward-char 1) ; ERB tags *always* include delims
           (let ((erb-info (save-excursion (rhtml-scan-erb-tag))))
             (when (car erb-info)
               (setq tag-type (car erb-info))
               (setq name (cdr erb-info)))))
          ;; /PST
          (t				; open or empty tag
	   (setq tag-type 'open
		 name (sgml-parse-tag-name))
	   (if (or (eq ?/ (char-before (- tag-end 1)))
		   (sgml-empty-tag-p name))
	       (setq tag-type 'empty))))))
      (goto-char tag-start)
      (sgml-make-tag tag-type tag-start tag-end name))))


;; PST -- ERB tags return useful stuff such as "erb-block"
(defun sgml-beginning-of-tag (&optional top-level)
  "Skip to beginning of tag and return its name.
If this can't be done, return nil."
  (let ((context (sgml-lexical-context)))
    (if (memq (car context) '(tag erb-open erb-close erb-middle)) ;; PST
	(progn
	  (goto-char (cdr context))
          ;; PST cond added for ERB
          (or (cdr (save-excursion (rhtml-scan-erb-tag)))
              (if (looking-at sgml-tag-name-re)
                  (match-string-no-properties 1))))
      (if top-level nil
	(when (not (eq (car context) 'text))
	  (goto-char (cdr context))
	  (sgml-beginning-of-tag t))))))


;; PST -- Added support for `erb-*' types
(defun sgml-lexical-context (&optional limit)
  "Return the lexical context at point as (TYPE . START).
START is the location of the start of the lexical element.
TYPE is one of `string', `comment', `tag', `cdata', `erb-*' or `text'.
Optional argument LIMIT is the position to start parsing from.
If nil, start from a preceding tag at indentation."
  (interactive) ;; PST
  (save-excursion
    (let ((pos (point))
	  text-start state)
      (if limit
          (goto-char limit)
        ;; Skip tags backwards until we find one at indentation
        (while (and (ignore-errors (sgml-parse-tag-backward))
                    (not (sgml-at-indentation-p)))))
      (with-syntax-table sgml-tag-syntax-table
	(while (< (point) pos)
	  ;; When entering this loop we're inside text.
	  (setq text-start (point))
	  (skip-chars-forward "^<" pos)
          (setq state
                (cond
                 ((= (point) pos)
                  ;; We got to the end without seeing a tag.
                  nil)
                 ((looking-at "<!\\[[A-Z]+\\[")
                  ;; We've found a CDATA section or similar.
                  (let ((cdata-start (point)))
                    (unless (search-forward "]]>" pos 'move)
                      (list 0 nil nil 'cdata nil nil nil nil cdata-start))))
                 ;; PST
                 ((looking-at rhtml-erb-tag-open-re)
                  (let ((erb-start (point))
                        (tag-type (car (rhtml-scan-erb-tag))))
                    (if tag-type
                        (list 0 nil nil tag-type nil nil nil nil erb-start)
                      (forward-char 1) ;not really an ERB tag, skip it (could cause <<tag>)?
                      nil)))
                 ;; /PST
                 (t
                  ;; We've reached a tag.  Parse it.
                  ;; FIXME: Handle net-enabling start-tags
                  (parse-partial-sexp (point) pos 0))))))
      (let ((lcon
             (cond
              ((eq (nth 3 state) 'cdata) (cons 'cdata (nth 8 state)))
              ((rhtml-erb-tag-type-p (nth 3 state)) (cons (nth 3 state) (nth 8 state))) ;; PST
              ((nth 3 state) (cons 'string (nth 8 state)))
              ((nth 4 state) (cons 'comment (nth 8 state)))
              ((and state (> (nth 0 state) 0)) (cons 'tag (nth 1 state)))
              (t (cons 'text text-start)))))
        lcon))))


;;;
;;; Hacks disabled by default
;;;
;;(rhtml-disable-sgml-hacks)

;;;
(provide 'rhtml-sgml-hacks)