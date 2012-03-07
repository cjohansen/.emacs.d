;;; oppdrag-mode.el --- Convenience utilities for working with Finn Oppdrag in Emacs

;; Copyright (C) 2011  Christian Johansen

;; Author: Christian Johansen <christian@moon>
;; Keywords:

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

;;; Code:

(defun oppdrag--string-replace (from to string &optional re)
  "Replace all occurrences of FROM with TO in STRING.
All arguments are strings.
When optional fourth argument is non-nil, treat the from as a regular expression."
  (let ((pos 0)
        (res "")
        (from (if re from (regexp-quote from))))
    (while (< pos (length string))
      (if (setq beg (string-match from string pos))
          (progn
            (setq res (concat res
                              (substring string pos (match-beginning 0))
                              to))
            (setq pos (match-end 0)))
        (progn
          (setq res (concat res (substring string pos (length string))))
          (setq pos (length string)))))
    res))

(defun oppdrag-hot-deploy-buffer-file ()
  "If the current buffer is visiting a file, and that file is deployed
in an exploded war, re-deploy the file."
  (interactive)
  (let* ((source (buffer-file-name))
         (target (oppdrag--string-replace "src/main/webapp" "target/oppdrag" source)))
    (if (and (file-writable-p target)
             (not (string= source target)))
        (progn
          (copy-file source target t)
          (message (concat "Deployed " source " to " target)))
      (message (concat target " does not exist, file not deployed")))))

(define-minor-mode oppdrag-mode
  "Convenience utilities for working with Finn Oppdrag"
  nil " Oppdrag" nil
  (if oppdrag-mode
      (add-hook 'after-save-hook 'oppdrag-hot-deploy-buffer-file nil t)
    (remove-hook 'after-save-hook 'oppdrag-hot-deploy-buffer-file t)))

(defun oppdrag--setup-js-quirks ()
  (when (string-match-p "oppdrag-services" (buffer-file-name))
    (setq js2-additional-externs '("FINN" "testCase" "cull" "buster" "_"))
    (setq buster-default-global "FINN.oppdrag")
    (setq buster-add-default-global-to-iife t)
    (setq buster-testcase-snippets-enabled nil)
    (make-variable-buffer-local 'buster-test-prefix)
    (setq buster-test-prefix "test should ")
    (set (make-local-variable 'sgml-basic-offset) 4)
    (make-variable-buffer-local 'js2-basic-offset)
    (setq js2-basic-offset 4)))

(add-hook 'js2-mode-hook 'oppdrag--setup-js-quirks)

(provide 'oppdrag-mode)
;;; oppdrag-mode.el ends here
