;;; treesitter-module.el --- settings for package treesitter
;;; Commentary:
;; Configure package treesitter
;;; Code:

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(provide 'treesitter-module)
;;; treesitter-module.el ends here
