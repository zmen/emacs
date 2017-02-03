(setq make-backup-files nil)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

(delete-selection-mode 1)

(global-auto-revert-mode 1)

(setq auto-save-default nil)

(setq ring-bell-function 'ignore)

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-improve-default)
