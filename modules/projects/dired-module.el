;;; dired-module.el --- settings for package dired
;;; Commentary:
;; Configure package Dired
;;; Code:

(use-package all-the-icons)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(provide 'dired-module)
;;; dired-module.el ends here
