(require 'regex-dsl)

(defvar jmc-bracket-face 'jmc-bracket-face
  "Face name to use for brackets in jmc-mode.")
(defface jmc-bracket-face
  '((((class color)) (:foreground "#666666"))
    (t (:foreground "gray100")))
  "Face for brackets in jmc-mode")

(setq jmc-mode-map (make-sparse-keymap))

(setq jmc-mode-syntax-table
      (let ((table (make-syntax-table)))
        (modify-syntax-entry ?\[ "w" table)
        (modify-syntax-entry ?\] " " table)
        (modify-syntax-entry ?{ "(" table)
        (modify-syntax-entry ?} ")" table)
        table))

(setq jmc-nop-regexp
      (redsl-to-regexp
       '(concat "#nop "
                (* (anychar))
                (line-end))))

(setq jmc-command-regexp
      (redsl-to-regexp
       '(concat "#"
                (or
                 (+ (char-set "0-9"))
                 "alias" "ali" "action" "act" "antisubstitute" "bell" "char"
                 "connect" "con" "cr" "echo" "drop" "gag" "group" "highlight" "hig"
                 "hotkey" "if" "ignore" "info" "killall" "log" "loop" "map" "mark"
                 "math" "message" "multiaction" "multihighlight" "nop" "path"
                 "pathdir" "presub" "read" "return" "savepath" "script" "showme"
                 "speedwalk" "status" "substitute" "sub" "textin" "tick" "tickon"
                 "tickoff" "tickset" "ticksize" "togglesubs" "tabadd" "tabdel"
                 "unaction" "unact" "unalias" "unantisub" "ungag" "unhotkey"
                 "unhighlight" "unpath" "unsubs" "unsub" "unvariable" "variable"
                 "var" "verbatim" "write" "zap"))))

(setq jmc-vars-regexp
      (redsl-to-regexp
       '(or (concat "$" (+ (word-char)))
            (concat (+ "%") (char-set "0-9")))))

(setq jmc-brackets-regexp
      (redsl-to-regexp
       '(char-set "{}")))

(setq jmc-ansi-code-regexp
      (redsl-to-regexp
       '(concat "["
                (+ (char-set "0-9"))
                (\? ";1")
                "m")))

(setq jmc-mode-font-lock-keywords
      (list
       (cons jmc-nop-regexp '(0 font-lock-comment-face))
       (cons jmc-command-regexp '(0 font-lock-builtin-face))
       (cons jmc-vars-regexp '(0 font-lock-variable-name-face))
       (cons jmc-brackets-regexp '(0 jmc-bracket-face))
       (cons jmc-ansi-code-regexp '(0 jmc-bracket-face))))

(defun jmc-mode ()
  "Major mode for editing JMC script files.
Special commands:
\\{jmc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'jmc-mode)
  (setq mode-name "JMC")
  (use-local-map jmc-mode-map)
  (set-syntax-table jmc-mode-syntax-table)
  (setq font-lock-defaults '(jmc-mode-font-lock-keywords))
  (run-hooks 'jmc-mode-hook))

(provide 'jmc-mode)
