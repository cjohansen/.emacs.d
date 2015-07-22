;;; core-async-mode.el

;; It requires core async functions and macros for you

(require 'dash)
(require 'clj-refactor)

(defvar core-async--functions
  (list
   "<!" "<!!" ">!" ">!!" "admix" "alt!" "alt!!" "alts!" "alts!!" "buffer"
   "chan" "close!" "do-alts" "dropping-buffer" "mix" "mult" "offer!"
   "onto-chan" "pipe" "pipeline" "pipeline-async" "pipeline-blocking"
   "poll!" "pub" "put!" "sliding-buffer" "solo-mode" "sub" "take!"
   "tap" "thread" "thread-call" "timeout" "to-chan" "unblocking-buffer?"
   "unmix" "unmix-all" "unsub" "unsub-all" "untap" "untap-all"))

(defvar core-async--macros
  (list "go" "go-loop"))

(defvar core-async--functions-re
  (concat "(" (regexp-opt (-concat core-async--functions
                                   core-async--macros)
                          'symbols)))

(defun core-async--find-usages ()
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward core-async--functions-re nil t)
        (!cons (cljr--find-symbol-at-point) result)))
    (-distinct result)))

(defun core-async--remove-from-ns (type s)
  (cljr--goto-ns)
  (when (cljr--search-forward-within-sexp (concat "(" type))
    (skip-syntax-forward " >")
    (while (not (looking-at ")"))
      (if (looking-at (regexp-quote (concat "[" s)))
          (cljr--delete-sexp)
        (paredit-forward))
      (skip-syntax-forward " >"))))

(defun core-async--update-clj-namespace ()
  (save-excursion
    (let ((usages (core-async--find-usages)))
      (core-async--remove-from-ns ":require" "clojure.core.async")
      (when usages
        (cljr--insert-in-ns ":require")
        (insert "[clojure.core.async :refer [")
        (apply 'insert (->> usages
                            (-sort cljr-sort-comparator)
                            (-interpose " ")))
        (insert "]]")))
    (cljr-sort-ns)))

(defun core-async--update-cljs-namespace ()
  (save-excursion
    (let* ((usages (core-async--find-usages))
           (used-fns (--filter (member it core-async--functions) usages))
           (used-macros (--filter (member it core-async--macros) usages)))
      (core-async--remove-from-ns ":require" "cljs.core.async")
      (core-async--remove-from-ns ":require-macros" "cljs.core.async.macros")
      (when used-fns
        (cljr--insert-in-ns ":require")
        (just-one-space)
        (insert "[cljs.core.async :refer [")
        (apply 'insert (->> used-fns
                            (-sort cljr-sort-comparator)
                            (-interpose " ")))
        (insert "]]"))
      (when used-macros
        (cljr--insert-in-ns ":require-macros")
        (just-one-space)
        (insert "[cljs.core.async.macros :refer [")
        (apply 'insert (->> used-macros
                            (-sort cljr-sort-comparator)
                            (-interpose " ")))
        (insert "]]"))
      (cljr-sort-ns))))

(defun core-async-update-namespace ()
  (interactive)
  (cond ((s-ends-with? ".clj" (buffer-file-name))
         (core-async--update-clj-namespace))

        ((s-ends-with? ".cljs" (buffer-file-name))
         (core-async--update-cljs-namespace))))

(define-minor-mode core-async-mode
  "Core async mode" nil " async" nil
  (if core-async-mode
      (add-hook 'before-save-hook 'core-async-update-namespace t t)
    (remove-hook 'before-save-hook 'core-async-update-namespace t)))

(provide 'core-async-mode)
