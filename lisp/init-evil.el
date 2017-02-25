(require 'evil)
(evil-mode 1)

(setq evil-default-state 'emacs)

;; Bind 'jk' to escape in evil insert mode by key-chord-mode
;; Note that 'jk' equals to 'kj' in this way
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

(provide 'init-evil)
