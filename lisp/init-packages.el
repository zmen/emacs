;; 1. set elpa.emacs-china as default melpa source
;; 2. install required third-party packages

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://elpa.emacs-china.org/melpa/") t))

(require 'cl)
(defvar my/packages '(
		      ;; --- Auto-completion ---
		      company
		      ;; --- Major Mode ---
		      js2-mode
		      web-mode
		      ;; --- Themes ---
		      monokai-theme
		      solarized-theme
		      spacemacs-theme
		      spaceline
		      ;; --- Layer ---
		      evil
		      key-chord ;; for 'jk' to 'escape'
		      ;; --- Others ---
		      smex
		      ) "Default packages")

(setq package-selected-packages my/packages)
(defun my/packages-installed-p ()
  (loop for pkg in my/packages
	when (not (package-installed-p pkg))
	do (return nil)
	finally (return t)))
(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'init-packages)
