;; <f2>: open init file
;; <f9>: loop alpha-list
;; C-x C-b: buffer-menu
;; C-c a: org-agenda
;; M-x: smex settings
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

;; set transparent effect
(setq alpha-list '((55 55) (100 100)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    )
  )
(global-set-key [(f9)] 'loop-alpha)

(global-set-key (kbd "C-x C-b") 'buffer-menu)

(global-set-key (kbd "C-c a") 'org-agenda)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'init-keybinding)
