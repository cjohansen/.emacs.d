;; FINN Oppdrag
(require 'oppdrag-mode)
(add-hook 'find-file-hook
          (lambda ()
            (when (string-match-p "oppdrag-services" (buffer-file-name))
              (oppdrag-mode))))

;; Zombie TDD
(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "zombietdd" (buffer-file-name))
              (setq js2-additional-externs '("ZOMBIE" "Faye"))
              (setq buster-default-global "ZOMBIE")
              (setq buster-add-default-global-to-iife t)
              (setq buster-use-strict t))))

(provide 'project-mappings)