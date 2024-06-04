;;; init.el --- initialize packages
;;; Commentary:
;; Initialize and load packages
;;; Code:

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package for non-linux
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load path for custom modules
(add-to-list 'load-path (expand-file-name "~/.emacs.d/core"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/keybinds"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/theme"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/completion/ivy"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/completion/company"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/lsp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/projects"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/evil"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/org"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/misc"))

;; Configure packages

(require 'core-editor)
(require 'general-module)
(require 'evil-module)
(require 'ivy-module)
(require 'magit-module)
(require 'projectile-module)
(require 'lsp-mode-module)
(require 'treesitter-module)
(require 'company-module)
(require 'treemacs-module)
(require 'orgmode-module)
(require 'misc-module)
(require 'theme-module)
(require 'modeline-module)

(setq flycheck-emacs-lisp-load-path 'inherit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-box company which-key solaire-mode smartparens rainbow-delimiters magit lsp-mode ivy-rich counsel-projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
