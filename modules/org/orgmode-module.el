;;; orgmode-module.el --- settings for org-mode
;;; Commentary:
;; Configure org mode
;;; Code:

(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-ellipsis "⤵")
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-indented t)
  (setq org-indent-mode t)
  (setq org-agenda-files '("~/org/agenda.org"))
  :general
  (leader-keys
    "ots" 'org-time-stamp))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Org")
  :config
  (org-roam-setup))

(use-package websocket
    :after org-roam)

(use-package org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package evil-org
  :commands evil-org-mode
  :after org
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
  :general
  (leader-keys
    "ni" 'org-roam-node-insert))

(provide 'orgmode-module)
;;; orgmode-module.el ends here
