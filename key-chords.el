(require 'key-chord)
(key-chord-mode 1)

;; Move to char similar to "f" in vim, f+g forward, d+f backward
(key-chord-define-global "fg" 'iy-go-to-char)
(key-chord-define-global "df" 'iy-go-to-char-backward)


(provide 'key-chords)