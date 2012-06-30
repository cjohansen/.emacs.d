(setq slime-js-target-url "http://localhost:3000")
(setq slime-js-swank-command "swank-js")
(setq slime-js-swank-args '())
(setq slime-js-browser-command "open -a Safari")
(setq slime-js-browser-jacked-in nil)

(defun slime-js-run-swank ()
  "Runs the swank side of the equation."
  (interactive)
  (unless (boundp 'slime-js-swank-buffer)
    (setq slime-js-swank-buffer
          (apply #'make-comint "swank-js"  slime-js-swank-command nil slime-js-swank-args))))

(defun slime-js-jack-in-node ()
  "Start a swank-js server and connect to it, opening a repl."
  (interactive)
  (slime-js-run-swank)
  (sleep-for 1)
  (setq slime-protocol-version 'ignore)
  (slime-connect "localhost" 4005))

(defun slime-js-jack-in-browser ()
  "Start a swank-js server, connect to it, open a repl, open a browser, connect to that."
  (interactive)
  (js2-jack-in-node)
  (sleep-for 2)
  (slime-js-set-target-url slime-js-target-url)
  (shell-command (concat slime-js-browser-command " http://localhost:8009"))
  (sleep-for 3)
  (setq slime-remote-history nil)
  (slime-js-select-remote (caadr (slime-eval '(js:list-remotes))))
  (setq slime-js-browser-jacked-in t))

(defadvice save-buffer (after save-css-buffer activate)
  (when (and slime-js-browser-jacked-in (eq major-mode 'css-mode))
    (slime-js-refresh-css)))

(defun js2-is-eval-friendly-node (n)
  (or (and (js2-stmt-node-p n) (not (js2-block-node-p n)))
      (and (js2-function-node-p n) (js2-function-node-name n))))

(defun slime-js-eval-statement ()
  (interactive)
  (let ((stmt (js2r--closest 'js2-is-eval-friendly-node)))
    (slime-flash-region (js2-node-abs-pos stmt) (js2-node-abs-end stmt))
    (slime-js-eval
     (js2-node-string stmt)
     #'(lambda (s) (message (cadr s))))))

(define-key slime-js-minor-mode-map (kbd "C-x C-e") 'slime-js-eval-statement)

(provide 'setup-slime-js)
