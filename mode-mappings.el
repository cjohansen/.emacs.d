;; YAML
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("jsTestDriver\\.conf$" . yaml-mode))

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
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . javascript-mode))

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
(autoload 'buster-mode "buster-mode")
(add-file-find-hook-with-pattern "test\\.js$" (lambda () (buster-mode)) "require(\\(\"\\|'\\)buster")

;; Markdown
;;(require 'showoff-mode)
(autoload 'markdown-mode "markdown-mode")
(autoload 'showoff-mode "showoff-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . showoff-mode))

;; Plain HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

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