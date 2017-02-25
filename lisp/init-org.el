(setq org-src-fontify-natively t)

(setq org-agenda-files '("~/org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.emacs.d/gtd.org" "Work")
	 "* TODO [#B] %?\n %i\n"
	 :empty-lines 1)))

(provide 'init-org)
