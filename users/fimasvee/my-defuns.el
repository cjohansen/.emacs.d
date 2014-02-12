(defun wte--unique-filename (stub &optional index)
  (setq index (or index 1))
  (let ((filename (concat "~/stuff/what-the-emacsd/resources/posts/"
                          stub
                          ".el"
                          (if (< index 10) "-0" "-")
                          (number-to-string index)
                          ".html")))
    (if (file-exists-p filename)
        (wte--unique-filename stub (1+ index))
      filename)))

(defun what-the-emacsd-post (beg end)
  (interactive "r")
  (let ((example (with-current-buffer (htmlize-region beg end)
                    (search-forward "<pre>")
                    (setq beg (point))
                    (search-forward "</pre>")
                    (forward-char -7)
                    (buffer-substring beg (point))))
         (filename (wte--unique-filename (buffer-file-name-body)))
         (timestamp (floor (time-to-seconds (current-time)))))
    (find-file filename)
    (insert (format "<!-- %S -->

<p></p>

<hr/>

<pre class=\"code-snippet\">%s</pre>

<hr/>

<p></p>
" timestamp example))
    (goto-char 25)))

(defun what-the-emacsd-publish ()
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (save-buffer)
    (compile
     (format "git add %s && git ci -m %S && git push"
             filename
             filename) t)))

(defun -flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'priority 100)
    (run-with-timer 0.2 nil 'delete-overlay overlay)))

(defun js2r--flash-node (node)
  (-flash-region (js2-node-abs-pos node)
                 (js2-node-abs-end node))
  (js2-node-short-name node))

;; run clojure.test at point, with fixture (hardcoded I know)

(defun run-deftest-at-point ()
  (interactive)
  (save-excursion
    (search-backward "deftest")
    (search-forward "(")
    (backward-char)
    (let* ((beg (point))
           (end (forward-list))
           (contents (buffer-substring-no-properties beg end)))
      (nrepl-find-and-clear-repl-buffer)
      (nrepl-interactive-eval
       (concat "(h/reset-state-fixture (fn [] " contents "))")))))

(provide 'my-defuns)
