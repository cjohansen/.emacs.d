(require 'package)

;; Add marmalade to package repos
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defun packages-install (&rest packages)
  (mapc (lambda (package)
          (when (not (package-installed-p package))
            (package-install package)))
        packages))

(provide 'setup-package)
