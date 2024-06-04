;;; lsp-mode-module.el --- settings for lsp
;;; Commentary:
;; Configure lsp mode
;;; Code:

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.css?\\'" "\\.js?\\'" "\\.blade\\.php\\'")
  :config
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing t))

(use-package php-mode
  :ensure t)

(use-package robot-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook
  ((web-mode . lsp-deferred)
   (c-ts-mode . lsp-deferred)
   (php-mode . lsp-deferred)
   (robot-mode . lsp-mode)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred
  :custom
  gc-cons-threshold (* 100 1024 1024)
  read-process-output-max (* 1024 1024)
  create-lockfiles nil
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(robot-mode . "robot"))
    (lsp-register-client (make-lsp-client
			  :new-connection (lsp-stdio-connection "robotframework-lsp")
			  :activation-fn (lsp-activate-on "robot")
			  :server-id 'robotframework-lsp))))

(use-package prettier
  :ensure t
  :hook ((web-mode . prettier-mode)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(provide 'lsp-mode-module)
;;; lsp-mode-module.el ends here
