;;; evil-module.el --- settings for evil mode
;;; Commentary:
;; Configure package evil and other related packages
;;; Code:

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :config
  (evil-mode 1)
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(provide 'evil-module)
;;; evil-module.el ends here
