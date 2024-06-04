;;; magit-module.el --- settings for package magit
;;; Commentary:
;; Configure package magit
;;; Code:

(use-package magit
  :ensure t
  :general
  (leader-keys
    "gs" 'magit))

(provide 'magit-module)
;;; magit-module.el ends here
