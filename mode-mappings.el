;; YAML mode for jsTestDriver.conf plz
(add-to-list 'auto-mode-alist '("jsTestDriver\\.conf$" . yaml-mode))

;; Ruby mode
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))

;; Showoff mode
(add-to-list 'auto-mode-alist '("\\.md$" . showoff-mode))

;; Plain HTML mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
