(tool-bar-mode -1) ;; Close tool bar
(scroll-bar-mode -1) ;; Close scroll bar
(global-hl-line-mode 1) ;; Highlight current line
(global-linum-mode 1) ;; Always show line
(setq inhibit-splash-screen 1) ;; Remove welcome page

;; Themes
(load-theme 'monokai t)
(require 'org)
(setq org-src-fontify-natively t)

;; Status bar
(require 'spaceline-config)
(spaceline-spacemacs-theme)

(provide 'init-ui)
