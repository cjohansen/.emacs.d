(require 'js2-mode)

;; Expand and contract object
(key-chord-define js2-mode-map "u8"  'js-expand-object)
(key-chord-define js2-mode-map "i9"  'js-contract-object)

;; Add semicolon to end of line
(key-chord-define js2-mode-map ";;"  "\C-e;")

;; js2-mode steals TAB, let's steal it back for yasnippet
(define-key js2-mode-map (kbd "TAB") (lambda()
                                            (interactive)
                                            (let ((yas/fallback-behavior 'return-nil))
                                              (unless (yas/expand)
                                                (indent-for-tab-command)
                                                (if (looking-back "^\s*")
                                                    (back-to-indentation))))))

(provide 'js2-mode-expansions)