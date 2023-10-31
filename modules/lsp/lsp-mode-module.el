;;; lsp-mode-module.el --- settings for lsp
;;; Commentary:
;; Configure lsp mode
;;; Code:

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.css?\\'")
  :config
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t))

(use-package lsp-mode
  :hook
  ((web-mode . lsp-deferred)
  (js-mode . lsp-deferred)
   (typescript-mode . lsp-deferred)
   (c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred
  :init
  (setq lsp-language-server 'typescript-ls)
  (setq lsp-clients-typescript-server-args '("--stdio"))
  :config
  (setq lsp-clients-clangd-executable "clangd")
  :custom
  gc-cons-threshold (* 100 1024 1024)
  read-process-output-max (* 1024 1024)
  create-lockfiles nil)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(provide 'lsp-mode-module)
;;; lsp-mode-module.el ends here
