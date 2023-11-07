;;; modeline-module.el --- settings for package doom-modeline
;;; Commentary:
;; Configure package doom-modeline
;;; Code:

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

(use-package hide-mode-line
 :ensure t
 :hook (treemacs-mode . hide-mode-line-mode))

(provide 'modeline-module)
;;; modeline-module.el ends here
