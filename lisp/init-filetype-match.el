(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
	 ("\\.C\\'" . c++-mode))
       auto-mode-alist))

(provide 'init-filetype-match)
