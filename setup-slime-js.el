(setq slime-js-target-url "http://localhost:3000")
(setq slime-js-swank-command "swank-js")
(setq slime-js-swank-args '())
(setq slime-js-browser-command "open -a Safari")

(defun slime-js-run-swank ()
  "Runs the swank side of the equation."
  (interactive)
  (unless (boundp 'slime-js-swank-buffer)
          (setq slime-js-swank-buffer
                (apply #'make-comint "swank-js"  slime-js-swank-command nil slime-js-swank-args))))

(defun js2-jack-in-node ()
  (interactive)
  (slime-js-run-swank)
  (sleep-for 1)
  (setq slime-protocol-version 'ignore)
  (slime-connect "localhost" 4005))

(defun js2-jack-in-browser ()
  (js2-jack-in-node)
  (sleep-for 2)
  (slime-js-set-target-url slime-js-target-url)
  (shell-command (concat slime-js-browser-command " http://localhost:8009"))
  (sleep-for 3)
  (setq slime-remote-history nil)
  (slime-js-select-remote (caadr (slime-eval '(js:list-remotes)))))

(provide 'setup-slime-js)
