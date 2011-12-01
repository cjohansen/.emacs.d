(require 'key-chord)
(key-chord-mode 1)

;; Move to char similar to "f" in vim, f+j forward, f+h backward
(key-chord-define-global "fj" 'iy-go-to-char)
(key-chord-define-global "fh" 'iy-go-to-char-backward)

;; Indent entire buffer and fix whitespace
(key-chord-define-global "i0" 'indent-buffer)
(key-chord-define-global "i9" 'whitespace-cleanup)


(provide 'key-chords)