;;; misc-module.el --- settings for miscellaneous modules
;;; Commentary:
;; Configure miscelaneous packages
;;; Code:

(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

(when (or (eq system-type 'gnu/linux) (eq system-type 'darwin))
  (use-package vterm
    :ensure t))

(provide 'misc-module)
;;; misc-module.el ends here
