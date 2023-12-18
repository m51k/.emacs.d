;;; lsp-mode-module.el --- settings for lsp
;;; Commentary:
;; Configure lsp mode
;;; Code:

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.css?\\'" "\\.js?\\'" "\\.jsx?\\'" "\\.tsx?\\'")
  :config
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t))

(use-package lsp-mode
  :hook
  ((web-mode . lsp-deferred)
   (c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred
  :config
  (setq lsp-clients-clangd-executable "clangd")
  :custom
  gc-cons-threshold (* 100 1024 1024)
  read-process-output-max (* 1024 1024)
  create-lockfiles nil)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package prettier-js)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))
(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.tsx?\\'" . prettier-js-mode))))

(provide 'lsp-mode-module)
;;; lsp-mode-module.el ends here
