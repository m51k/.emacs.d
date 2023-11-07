;;; theme-module.el --- settings for package doom-themes
;;; Commentary:
;; Configure package doom-themes
;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config))

(use-package autothemer
 :ensure t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/modules/theme")
(load-theme 'oxocarbon t)

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'theme-module)
;;; theme-module.el ends here
