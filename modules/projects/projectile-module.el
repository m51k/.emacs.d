;;; projectile-module.el --- settings for package projectile
;;; Commentary:
;; Configure package projectile
;;; Code:

(use-package projectile
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  ;; Project folder
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(provide 'projectile-module)
;;; projectile-module.el ends here
