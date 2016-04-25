(require 'cider-interaction)
(require 'ansi-color)

(setq clj-autotest-run-expectations
      "(do
        (require 'clojure.tools.namespace.repl)
        (require 'expectations)
        (reset! expectations/run-tests-on-shutdown false)

        (let [all (->> (all-ns)
                       (mapcat (comp vals ns-interns)))
              previously-ran-tests (filter (comp :expectations/run meta) all)]
          (doseq [test previously-ran-tests]
            (alter-meta! test dissoc :expectations/run :status)))

        (let [result (binding [*out* (new java.io.StringWriter)]
                       (clojure.tools.namespace.repl/refresh))]
          (if (= :ok result)
            (expectations/run-all-tests)
            (println \"Error refreshing environment: \" clojure.core/*e))))")

(defun clja--clear-buffer ()
  (delete-region (point-min) (point-max)))

(defun clja--run-expectations ()
  (nrepl-sync-request:eval clj-autotest-run-expectations))

(defun clja--get-output (result)
  (s-trim (nrepl-dict-get result "out")))

(defun clja--insert-output (result)
  (insert (clja--get-output result)))

(defun clja--ansi-colorize-buffer ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun clja--fit-window-snuggly (max-height)
  (window-resize nil (- (max 4 (min max-height
                                    (1+ (line-number-at-pos (point-max)))))
                        (window-height))))

(defun clja--recenter-bottom ()
  (recenter (- -1 (min (max 0 scroll-margin)
                       (truncate (/ (window-body-height) 4.0))))))

(defvar clja--buffer "*clj-autotest*")

(defmacro with-clja-buffer (&rest body)
  `(let ((current (current-buffer))
         (window (get-buffer-window "*clj-autotest*")))
     (if window
         (select-window window)
       (let ((window (split-window-vertically -4)))
         (select-window window)
         (switch-to-buffer clja--buffer)
         (set-window-dedicated-p window t)))
     ,@body
     (switch-to-buffer-other-window current)))

(defun clja--run-tests ()
  (with-clja-buffer
   (clja--clear-buffer)
   (clja--insert-output
    (clja--run-expectations))
   (clja--ansi-colorize-buffer)
   (clja--fit-window-snuggly 10)
   (goto-char (point-min))
   (if (looking-at "Error refreshing environment")
       (search-forward "cause")
     (goto-char (point-max))
     (clja--recenter-bottom))))

(defun clja--run-tests-hook ()
  (when (and (bound-and-true-p cider-mode)
             (s-ends-with? ".clj" (buffer-file-name)))
    (clja--run-tests)))

(defun clj-autotest-init ()
  (interactive)
  (add-hook 'after-save-hook 'clja--run-tests-hook))

(defun clj-autotest-stop-it ()
  (interactive)
  (remove-hook 'after-save-hook 'clja--run-tests-hook)
  (kill-buffer clja--buffer))

(provide 'clj-autotest)
