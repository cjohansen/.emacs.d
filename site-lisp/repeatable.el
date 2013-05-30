;;; repeatable.el --- modify commands to repeat on last key event

;; Copyright 2005-2006 by Martin Blais, Stefan Monnier.

;; :Authors: Martin Blais <blais@furius.ca>,
;;           Stefan Monnier <monnier@iro.umontreal.ca>
;; :Revision: Revision: 4509
;; :Date: $Date: 2006-04-19 19:21:21 -0400 (Wed, 19 Apr 2006) $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 2,
;; as published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License version 2
;; along with this program and available at
;; http://docutils.sf.net/licenses/gpl.txt and at
;; http://www.gnu.org/licenses/gpl.txt.

;;; Commentary
;;; ==========

;; This package allows you to modify commands (via advice, or
;; wrapping the function) so that if the last key event that
;; triggered the command is repeated, the command is run again.

;;; Description
;;; ===========

;; This package allows you to modify commands (via advice, or wrapping
;; the function) so that if the last key event that triggered the
;; command is repeated, the command is run again.
;;
;; This is very useful for commands such as call-last-kbd-macro (C-x e),
;; where you can type "C-x e e e e e e e ..." to repeat the macro
;; multiple times interactively (this makes it easier to repeat, you
;; don't have to retype C-x).  Other examples include enlarge-window,
;; next-error, functions that move forward by pages, or that shift
;; regions of text to the right or left, etc.  Anything that benefits
;; from incremental adjustment.
;;
;; See also: the 'repeat' command (C-x z).

;;; Download
;;; ========

;; Click `Here <repeatable.el>`_ for download.

;;; SETUP

;; Set it up like this:
;;
;; * To make all bindings to a function repeatable on the last key
;;   binding, do this::
;;
;;     (repeatable-command-advice FUN)
;;
;; * To create a repeatable wrapper function, you can do something
;;   like this::
;;
;;     (global-set-key ...  (repeatable-command-def 'FUN))
;;
;; * To modify a keymap to make some of its bindings repeatable, do this::
;;
;;     (repeatable-substitute-binding FUN KEYMAP)
;;

;;; Ideas

;; * Maybe we should check if the key sequence consists in a single
;;   char, and if it does, do not enter the loop at all.  This
;;   would allow us to use the advice everywhere and get rid of the
;;   wrappers.

;;; Bugs

;; * There is a bug where if the next command is not for self-insert it does
;;   work properly (old comment, can't remember?).
;;
;; * Figure out why neither of these two work::
;;
;;     (repeatable-command-advice zap-to-char)
;;      (substitute-key-definition 'zap-to-char
;;                                 (repeatable-command-def 'zap-to-char)
;;                                 (current-global-map))
;;
;;      (repeatable-substitute-binding 'advertised-undo)
;;
;;      This currently doesn't work because we suspect that the advertised-undo
;;      command uses its name as the last function called to find out if it is being
;;      repeated and so since we wrap it with a macro it gets stuck by itself.  Try
;;      it.  Too bad, it would be nice to be able to do C-x u u u u u u (useful in a
;;      terminal window over a connection).

;;; History:

;;; Code:


(require 'advice)

(defvar repeatable-end-chars '(return)
  "Characters that end a repeatable key sequence loop.
Those do not cause insertion of themselves.")

(defvar repeatable-repeated nil
  "(Dynamic) variable that is set to 't when a command is invoked
in a repeated fashion. The first time the command is invoked,
this is not set.")

;; This following version modified by Stefan Monnier.
(defmacro repeatable-command-advice (fun &optional exit-fun eval-after)
  "Macro that will add an advice to the given command without replacing it.
By using the advice you can bind the repeatable concept against a
function rather than its keybinding.  FUN is the command to make
repeatable, EXIT-FUN is a function to run on exit and EVAL-AFTER
is a form to run after each repetition."

  ;; (This is an advice version of the macro definition above.
  `(defadvice ,fun (around repeat-with-last-key activate)
     "Make the operation repeatable by repeating the last char of the binding."
     (if (interactive-p)
         ;; FIXME: we'd want to check that the key binding had a length greater
         ;; than 1, but it's not easy.  `this-command-keys' is almost what we
         ;; want, unless the interactive spec used read-key-sequence or some
         ;; minibuffer input.
         (let ((repeatable-char last-command-event)
               (repeatable-prefix-chars (substring (this-command-keys) 0 -1))
               (repeatable-repeated nil)
               repeatable-last-event
               )
           (while (progn
                    ad-do-it
                    ,eval-after
                    (setq repeatable-last-event (read-event))
                    (and (not (member repeatable-last-event
                                      repeatable-end-chars))
                         (equal repeatable-last-event repeatable-char))
                    )
             ;; Make each repetition undo separately.
             (undo-boundary)
             ;; Allow the command to see if it is being called repeatedly.
             (setq repeatable-repeated t)
             )

           (unless (member repeatable-last-event repeatable-end-chars)

             ;; Check if the same key sequence would cause the other function to be
             ;; invoked, and if so, insert the prefix of the previous command to
             ;; cause the next command to be parsed with the same prefix, thus
             ;; entering its own advice (if it has one).
             (let ((keys (concatenate 'vector repeatable-prefix-chars
                                      (list repeatable-last-event))))

               (if (and ,exit-fun
                        (eq (key-binding keys) ,exit-fun))

                   ;; Place the whole new event on the stack.
                   (setq unread-command-events
                         (append (listify-key-sequence keys)
                                 unread-command-events))

                 ;; Do not use listify-key-sequence here, it puts the event
                 ;; in the buffer, don't know why.
                 (push repeatable-last-event unread-command-events)
                 ))
             ))
       ad-do-it)
     ))


;;-----------------------------------------------------------------
;; Command wrappers (instead of advice).
;; You should try to use the advice instead where it makes sense.

(defun repeatable-command (fun &optional otherkey)
  "Make any multi-sequence command FUN repeatable with its last char.
\\[keyboard-quit] or <ENTER> will exit silently, while any other
character than the last except OTHERKEY will also exit but insert
itself."
  (interactive)
  (let ((repeatable-char last-command-event)
        ev)
    (call-interactively fun)
    (while (or (and (setq ev (read-event)) nil)
               (equal ev repeatable-char)
               (equal ev (event-convert-list (list otherkey))))
      ;; Make each repetition undo separately.
      (undo-boundary)
      (call-interactively fun) )
    (if (not (member last-input-event repeatable-end-chars))
        (setq unread-command-events
              ;; do not use listify-key-sequence here, it puts the
              ;; event in the buffer, don't know why.
              (list last-input-event)
              )
      )))

(defmacro repeatable-command-def (fun)
  "Macro to easily create a wrapper function that is repeatable.
FUN is the function to wrap."
  `(lambda () (interactive) (repeatable-command ,fun)))

(defun repeatable-substitute-binding (fun &optional keymap)
  "Substitute the binding for FUN in KEYMAP to become a repeatable command."
  (let ((km (or keymap (current-global-map))))
    (substitute-key-definition
     fun
     `(lambda () (interactive) (repeatable-command (quote ,fun)))
     km)
    ))

;;-----------------------------------------------------------------

(provide 'repeatable)




;;-----------------------------------------------------------------

;; Code for testing cross-binding two repeatable commands:
;;
;; (progn (ad-unadvise 'insert-test-stuff-1)
;;        (ad-unadvise 'insert-test-stuff-2)
;;        (repeatable-command-advice insert-test-stuff-1
;;                                       'insert-test-stuff-2
;;                                       )
;;        (repeatable-command-advice insert-test-stuff-2
;;                                       'insert-test-stuff-1
;;                                       ))
;;
;;
;;      (print (list "inunread "(this-command-keys) )) ;; FIXME remove
;;
;;
;; (progn
;;   (global-set-key [(control x)(control l)]
;;                '(lambda () (interactive)
;;                   (print (this-command-keys))
;;                   (print last-input-event)))
;;   (setq unread-command-events (listify-key-sequence "")))
;;
;;
;;       (append (listify-key-sequence "l") unread-command-events))@@
;;
;;
;; (defun insert-test-stuff-1 () (interactive) (insert "@@"))
;; (defun insert-test-stuff-2 () (interactive) (insert "##"))
;;
;; (global-set-key [(control x)(l)] 'insert-test-stuff-1)
;; (global-set-key [(control x)(p)] 'insert-test-stuff-2)

;;; repeatable.el ends here
