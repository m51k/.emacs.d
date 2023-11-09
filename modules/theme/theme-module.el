;;; theme-module.el --- settings for package doom-themes
;;; Commentary:
;; Configure package doom-themes
;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
(use-package autothemer
 :ensure t)

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'theme-module)
;;; theme-module.el ends here
