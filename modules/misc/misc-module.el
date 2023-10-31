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

(provide 'misc-module)