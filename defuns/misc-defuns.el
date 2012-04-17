;; Misc defuns go here
;; It wouldn't hurt to look for patterns and extract once in a while

(defmacro create-simple-keybinding-command (name key)
  `(defmacro ,name (&rest fns)
     (list 'global-set-key (kbd ,key) `(lambda ()
                                         (interactive)
                                         ,@fns))))

(create-simple-keybinding-command f2 "<f2>")
(create-simple-keybinding-command f5 "<f5>")
(create-simple-keybinding-command f6 "<f6>")
(create-simple-keybinding-command f7 "<f7>")
(create-simple-keybinding-command f8 "<f8>")
(create-simple-keybinding-command f9 "<f9>")
(create-simple-keybinding-command f10 "<f10>")
(create-simple-keybinding-command f11 "<f11>")
(create-simple-keybinding-command f12 "<f12>")

(defun isearch-yank-selection ()
  "Put selection from buffer into search string."
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (isearch-yank-internal (lambda () (mark))))

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun linkify-region-from-kill-ring (start end)
  (interactive "r")
  (let ((text (buffer-substring start end)))
    (delete-region start end)
    (insert "<a href=\"")
    (yank)
    (insert (concat "\">" text "</a>"))))

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory dotfiles-dir 0))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun add-file-find-hook-with-pattern (pattern fn &optional contents)
  "Add a find-file-hook that calls FN for files where PATTERN
matches the file name, and optionally, where CONTENT matches file contents.
Both PATTERN and CONTENTS are matched as regular expressions."
  (lexical-let ((re-pattern pattern)
                (fun fn)
                (re-content contents))
    (add-hook 'find-file-hook
              (lambda ()
                (if (and
                     (string-match re-pattern (buffer-file-name))
                     (or (null re-content)
                         (string-match re-content
                                       (buffer-substring (point-min) (point-max)))))
                    (apply fun ()))))))

;; Fix kmacro-edit-lossage, it's normal implementation
;; is bound tightly to C-h
(defun kmacro-edit-lossage ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro 'view-lossage))
