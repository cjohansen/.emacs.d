(autoload 'gtags-mode "gtags" "" t)

(defun gtags-create-or-update ()
  "Create or update the gnu global tag file."
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p")))
      (let ((olddir default-directory)
            (topdir (read-directory-name
                     "gtags: top of source tree:" default-directory)))
        (cd topdir)
        (shell-command "GTAGSLABEL=rtags gtags && echo 'Created tagfile'")
        (cd olddir))
    (shell-command "GTAGSLABEL=rtags global -u && echo 'Updated tagfile'")))

(add-hook 'gtags-mode-hook
          (lambda()
            (local-set-key (kbd "M-.") 'gtags-find-tag)
            (local-set-key (kbd "M-,") 'gtags-find-with-grep)
            (local-set-key (kbd "M-:") 'gtags-find-symbol)
            (local-set-key (kbd "M-_") 'gtags-find-rtag)))

(provide 'gtags-tweaks)
