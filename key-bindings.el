;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand)

;; Expand region (increases selected region by semantic units)
(global-set-key (kbd "C-@") 'er/expand-region)

;; Repeat last command - too cumbersome with C-x z
(global-set-key (kbd "M-z") 'repeat)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Use shell-like backspace C-h, rebind help to F1
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
(global-set-key (kbd "<f1>") 'help-command)

;; Use C-w for backward-kill-word if no active region
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)

;; Make zap-to-char more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'zap-to-char)
(global-set-key (kbd "C-x C-z") 'suspend-frame)

;; M-m: Move to char - similar to "f" in vim
(global-set-key (kbd "M-m") 'iy-go-to-char)
(global-set-key (kbd "M-M") 'iy-go-to-char-backward)

;; Remap old M-m to M-i (better mnemonic for back-to-indentation)
;; We lose tab-to-tab-stop, which is no big loss in my use cases.
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Font size
(define-key global-map (kbd "C-+") 'zoom-in)
(define-key global-map (kbd "C--") 'zoom-out)

;; Create new frame (bound to regular mac-command)
(define-key global-map (kbd "M-n") 'make-frame-command)

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

;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
(global-set-key (kbd "C-x -") 'swap-windows)

;; Indentation help
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x h") 'view-url)

;; Help should search more than just commands
(global-set-key (kbd "<f1> a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Navigation bindings
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)

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

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

;; Recall position
(global-set-key (kbd "C-c C-s") 'toggle-buffer-pos)

;; Mark all
(global-set-key (kbd "C-c a") 'mark-whole-buffer)

;; Delete region (don't put it in the kill-ring)
(global-set-key (kbd "C-c C-w") 'delete-region)

;; Eval lines
(global-set-key (kbd "C-c C-e") 'eval-current-line)
(global-set-key (kbd "C-c M-e") 'eval-output-marked-lines)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status-fullscreen)

;; New lines
(global-set-key (kbd "<C-return>") 'new-line-below)
(global-set-key (kbd "<M-return>") 'new-line-in-between)
(global-set-key (kbd "C-c d") 'duplicate-line)

;; Line movement
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

;; Mark words, sentences and paragraphs
(global-set-key (kbd "M-1") 'mark-whole-word)
(global-set-key (kbd "M-2") 'mark-whole-word-command)

;; Fancy yank
(global-set-key (kbd "C-S-y") 'yank-as-line)

;; Toggle quotes
(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Sorting
(global-set-key (kbd "M-s l") 'sort-lines)

;; Paste URLs as HTML links
(global-set-key (kbd "C-c C-l") 'linkify-from-kill-ring)

;; Touch buffer file
(global-set-key (kbd "C-x t") 'touch-buffer-file)

;; Easy-mode rgrep
(global-set-key (kbd "M-s s") 'rgrep)

(provide 'key-bindings)
