;;; theme-module.el --- settings for package doom-themes
;;; Commentary:
;; Configure package doom-themes
;;; Code:

(use-package ef-themes
  :ensure t
  :init
  (setq ef-dark-palette-overrides
	'((bg-main "#111111")))
  :config
  (load-theme 'ef-dark :no-confirm))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'theme-module)
;;; theme-module.el ends here
