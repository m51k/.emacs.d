;;; orgmode-module.el --- settings for org-mode
;;; Commentary:
;; Configure org mode
;;; Code:

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  (setq org-ellipsis "⤵")
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-indented t)
  (setq org-indent-mode t))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'orgmode-module)
;;; orgmode-module.el ends here
