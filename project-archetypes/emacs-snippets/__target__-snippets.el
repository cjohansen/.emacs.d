;;; __target__-snippets.el --- Yasnippets for __name__

;; Copyright (C) __year__ Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: snippets
;; Version: 0.1.0
;; Package-Requires: ((s "1.4.0") (dash "1.2.0") (yasnippet "0.6.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Yasnippets for [__name__](__website__).
;;
;; Please visit README.md for more commentary.

;;; Code:

;;;###autoload
(defun __target__-snippets-show-docs-at-point ()
  (interactive)
  (__target__-snippets/show-docs (__target__-snippets/closest-__target__-identifer)))

(defvar __target__-snippets/docstrings
  '(("key" . "docstring")
    ("key" . "docstring")))

(defvar __target__-snippets/docstrings-regexp
  (regexp-opt (-map 'car __target__-snippets/docstrings)))

(defun -aget (alist key)
  (cdr (assoc key alist)))

(defun __target__-snippets/show-docs (id)
  (message (-aget __target__-snippets/docstrings id))
  nil)

(defun __target__-snippets/closest-__target__-identifer ()
  (save-excursion
    (search-forward " ")
    (search-backward-regexp __target__-snippets/docstrings-regexp)
    (match-string-no-properties 0)))

(setq __target__-snippets-root (file-name-directory (or load-file-name
                                                     (buffer-file-name))))

;;;###autoload
(defun __target__-snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" __target__-snippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load "yasnippet"
  '(__target__-snippets-initialize))

(provide '__target__-snippets)
;;; __target__-snippets.el ends here
