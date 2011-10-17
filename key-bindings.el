;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-|") 'hippie-expand)
(global-set-key (kbd "C-.") 'hippie-expand)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key [f1] 'menu-bar-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
(global-set-key (kbd "C-x -") 'swap-windows)

;; Indentation help
(global-set-key (kbd "C-x ^") 'join-line)

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x h") 'view-url)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Navigation bindings
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; Convenience on ThinkPad Keyboard: Use back/forward as pg up/down
(global-set-key (kbd "<XF86Back>") 'scroll-down)
(global-set-key (kbd "<XF86Forward>") 'scroll-up)
(global-set-key (kbd "<XF86WakeUp>") 'beginning-of-buffer)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Comment out block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Uncomment block
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Indent region
(global-set-key (kbd "C-c M-i") 'indent-region)

;; Eval buffer
(global-set-key (kbd "C-c v") 'eval-buffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

;; Mark all
(global-set-key (kbd "C-c a") 'mark-whole-buffer)

;; Delete region (don't put it in the kill-ring)
(global-set-key (kbd "C-c C-w") 'delete-region)

;; Eval lines
(global-set-key (kbd "C-c C-e") 'eval-current-line)
(global-set-key (kbd "C-c M-e") 'eval-output-marked-lines)

;; Git
(autoload 'magit-status "magit")
(global-set-key (kbd "<f8>") 'magit-status)
(global-set-key (kbd "C-x m") 'magit-status)

(global-set-key (kbd "<C-return>") 'new-line-below)
(global-set-key (kbd "<M-return>") 'new-line-in-between)
(global-set-key (kbd "C-S-a") 'beginning-of-line-text)
(global-set-key (kbd "C-c d") 'duplicate-line)

;; Line movement
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

;; Mark words, sentences and paragraphs
(global-set-key (kbd "M-1") 'mark-whole-word)
(global-set-key (kbd "M-2") 'mark-whole-word-command)

;; Fancy yank
(global-set-key (kbd "C-S-y") 'yank-as-line)

(provide 'key-bindings)
