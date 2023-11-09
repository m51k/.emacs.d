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

(tree-sitter-require 'tsx)

;; TSX
(define-derived-mode typescript-tsx-mode web-mode "TypeScript/TSX")
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))

;; JSX
(define-derived-mode javascript-jsx-mode web-mode "JavaScript/JSX")
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . javascript-jsx-mode))
(add-to-list 'tree-sitter-major-mode-language-alist '(javascript-jsx-mode . jsx))

(provide 'treesitter-module)
;;; treesitter-module.el ends here
