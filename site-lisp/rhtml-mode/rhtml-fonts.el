;;;
;;; rhtml-fonts.el - font-lock-based fontification support for `rhtml-mode'
;;;

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;; 2007 MAR 22 - Rewrote to work with jit-lock-mode, cleanup
;; 2007 MAR 28 - PST: changed to GPL license from MPL (using font-lock code)


(defvar rhtml-in-erb-keywords
  '(;("\\([A-Z][0-9a-zA-Z_]*\\)" . (1 font-lock-type-face prepend))
    ("[^_]\\<\\(alias\\|and\\|begin\\|break\\|case\\|catch\\|class\\|def\\|do\\|elsif\\|else\\|fail\\|ensure\\|for\\|end\\|if\\|in\\|module\\|next\\|not\\|or\\|raise\\|redo\\|rescue\\|retry\\|return\\|then\\|throw\\|super\\|unless\\|undef\\|until\\|when\\|while\\|yield\\|render\\)\\>[^_]" .
     (1 font-lock-keyword-face prepend))
    ("\\(@[0-9a-zA-Z_]*\\)" . (1 font-lock-variable-name-face prepend))
    ("\\(:[0-9a-zA-Z_]*\\)" . (1 font-lock-constant-face prepend))))

(defvar rhtml-font-lock-syntactic-keywords
  '(("\\(<\\)!--" (1 "< b"))
    ("--[ \t\n]*\\(>\\)" (1 "> b"))
    ("\\(<\\)%" (1 "<"))
    ("%\\(>\\)" (1 ">"))
    "Override `sgml-mode' syntactic keywords to support ERb tags."))

(defun rhtml-activate-fontification ()
  "Activate font-lock fontification support for the current buffer."
  ;; PST: note `jit-lock-mode' seems to play okay with
  ;; `font-lock-mode' (but I'm still fighting with `font-lock-mode')

  (font-lock-mode t)
  (jit-lock-mode t)

  ;; PST -- ERb regions are treated syntactically as comments but have
  ;; their `face' text property cleared and are overwritten.
  (set (make-local-variable 'font-lock-syntactic-keywords)
       'rhtml-font-lock-syntactic-keywords)

  (add-hook 'jit-lock-functions 'rhtml-fontify-region t t))

(defun rhtml-fontify-buffer ()
  (interactive)
  (jit-lock-refontify))

(defun rhtml-fontify-erb-block (type begin end)
  (let ((delim-face (cdr (assoc type erb-type-to-delim-face)))
        (body-face (cdr (assoc type erb-type-to-face)))
        (open-start begin)
        (open-end (+ begin (if (eq type 'exec) 2 3)))
        (close-start (- end 2))
        (close-end end))
    ;; apply edging and base
    (font-lock-append-text-property open-start open-end 'face delim-face)
    (when body-face
      (font-lock-append-text-property open-end close-start 'face body-face))
    (font-lock-append-text-property close-start close-end 'face delim-face)
    ;; apply normal ERb fontification
    (when (not (eq type 'comment))
      (let ((font-lock-keywords rhtml-in-erb-keywords)
            (case-fold-search nil))
        (font-lock-fontify-keywords-region open-end close-start)))))

(defun rhtml-font-unfontify-region (beg end)
  "Taken from ``font-lock.el''. Similar to
`font-lock-default-unfontify-region' but does not clear syntactical
information. This is useful to keep syntactical state without the
colorization."
  (remove-list-of-text-properties
   beg end (append
	    font-lock-extra-managed-props
            '(face font-lock-multiline))))

(defun rhtml-fontify-region (begin end)
  ;; PST -- hack to greedily grab more ERb tags to update (ensures
  ;; that the current ERb tag is updated entirely)
  (save-excursion
    (goto-char begin)
    (search-backward rhtml-erb-open-delim nil t)
    (setq begin (point))
    (goto-char end)
    (search-forward rhtml-erb-close-delim nil t)
    (setq end (point)))
  ;; fontify ERb tags -- fontification has already been applied by
  ;; font-lock-mode for sgml-mode so we need to clear faces (but not
  ;; syntactical information)
  (mapc (lambda (i)
          (rhtml-font-unfontify-region (nth 1 i) (nth 2 i))
          (apply 'rhtml-fontify-erb-block i))
        (rhtml-erb-regions begin end)))


;; ERB faces - each type of ERB tag has it's own face properties

(defface erb-face
  '((((class color) (min-colors 88) (background dark))
     :background "#383838")
    (((class color) (min-colors 88) (background light))
     ;; :background "azure")
     :background "snow2")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "azure")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  '((t (:inherit font-lock-preprocessor-face :bold t :italic t)))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)

(defface erb-exec-face
  `((t (:inherit erb-face)))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-exec-delim-face
  `((t (:inherit erb-delim-face :weight bold)))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-out-face
  `((t (:inherit erb-face)))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-out-delim-face
  `((((background dark)) :foreground "#aaffff" :background "#383838")
    (t (:inherit erb-delim-face :weight bold :foreground "darkred")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-comment-face
  `((((background dark)) :foreground "lightgreen")
    (t (:inherit erb-face :weight bold :foreground "darkgreen")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-comment-delim-face
  `((((background dark)) :foreground "lightgreen")
    (t (:inherit erb-delim-face :weight bold :foreground "darkgreen")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)


(defvar erb-type-to-face
  '((exec . erb-exec-face)
    (out . erb-out-face)
    (comment . erb-comment-face)))

(defvar erb-type-to-delim-face
  '((exec . erb-exec-delim-face)
    (out . erb-out-delim-face)
    (comment . erb-comment-delim-face)))

;;
(provide 'rhtml-fonts)