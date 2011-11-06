;; YAML
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("jsTestDriver\\.conf$" . yaml-mode))

;; Adventur
(add-to-list 'load-path "~/.emacs.d/site-lisp/adventur-mode")
(autoload 'adventur-mode "adventur-mode")
(add-to-list 'auto-mode-alist '("A[0-9]+/A[0-9]+\\.txt$" . adventur-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jspf$" . html-mode))

;; Ruby
(autoload 'rhtml-mode "rhtml-mode")
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))

(require 'gtags-tweaks)
(let ((hook (lambda ()
              (gtags-mode t)
              (gtags-create-or-update))))
  (add-hook 'ruby-mode-hook hook)
  (add-hook 'rhtml-mode-hook hook))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; JavaScript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "TAB") (lambda()
                                            (interactive)
                                            (let ((yas/fallback-behavior 'return-nil))
                                              (unless (yas/expand)
                                                (indent-for-tab-command)
                                                (if (looking-back "^\s*")
                                                    (back-to-indentation))))))))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))

;; Oppdrag
(require 'oppdrag-mode)

(add-hook 'find-file-hook
          (lambda ()
            (let* ((file (buffer-file-name))
                   (len (length file)))
              (if (string-match-p "oppdrag-services" file) (oppdrag-mode)))))

;; Buster.JS
;(autoload 'buster-mode "buster-mode")
;(setq buster-node-executable "/usr/local/bin/node")
;(add-file-find-hook-with-pattern "test\\.js$" (lambda () (buster-mode)) "require(\\(\"\\|'\\)buster")

;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Plain HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

;; PHP
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Mustache
(autoload 'mustache-mode "mustache-mode")

;; Apache config
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

(provide 'mode-mappings)