(setq gc-cons-threshold (* 50 1000 1000))

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

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

(setq inhibit-startup-message 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)
(menu-bar-mode -1)

(set-face-attribute 'default nil :font "Iosevka Comfy" :height 100)

(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode)

(use-package no-littering
  :ensure t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer leader-keys
                          :states '(normal visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-SPC")
  (leader-keys
    "tt" 'eshell
    "fs" 'save-buffer
    "kb" 'kill-buffer
    "kk" 'kill-buffer-and-window
    "qq" 'quit-window
    "ff" 'find-file
    "ws" 'split-window-horizontally
    "wv" 'split-window-vertically
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "cc" 'comment-line
    "cr" 'comment-or-uncomment-region
    "op" 'dired-jump
    "s" 'consult-line
    "b" 'consult-buffer
    "oal" 'org-agenda-list
    "nl" 'org-roam-buffer-toggle
    "nf" 'org-roam-node-find))

(general-auto-unbind-keys)

(use-package evil
  :config
  (evil-mode 1)
  :diminish evil-mode
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :ensure t
  :config
  (evil-collection-init))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :config
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :ensure t
  :config
  (setq consult-narrow-key "<")
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :ensure t)

(use-package corfu
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("TAB" . corfu-insert)
              ([tab] . corfu-insert)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode))

(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode
  :general
  (leader-keys
    "gs" 'magit))

(use-package autorevert
  :delight auto-revert-mode)

(use-package projectile
  :config (projectile-mode)
  :diminish projectile-mode
  :init
  ;; Project folder
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired)
  :general
  (leader-keys
    "p" 'projectile-command-map))

(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            ))
(electric-pair-mode t)

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none)
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(robot-mode . "robot"))
    (lsp-register-client (make-lsp-client
                          :new-connection (lsp-stdio-connection "robotframework-lsp")
                          :activation-fn (lsp-activate-on "robot")
                          :server-id 'robotframework-lsp)))
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  ((lsp-completion-mode . my/lsp-mode-setup-completion)
   (web-mode . lsp-deferred)
   (php-mode . lsp-deferred)
   (robot-mode . lsp-mode)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)

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

(use-package prettier
  :ensure t
  :hook ((web-mode . prettier-mode)))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (global-flycheck-mode))

(use-package tree-sitter
  :ensure t
  :diminish tree-sitter-mode
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-agenda-files '("~/Org/agenda.org"))
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
  :diminish evil-org-mode
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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq evil-mode-line-format '(before . mode-line-front-space))
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client-mode
                mode-line-modified
                mode-line-remote
                mode-line-frame-indentifcation
                " "
                mode-line-buffer-identification
                "  "
                vc-mode
                " "
                mode-line-modes
                " "
                mode-line-misc-info
                mode-line-end-spaces))

(use-package moe-theme
  :ensure t
  :config
  (load-theme 'moe-dark :no-confirm))

(use-package diminish
  :diminish (abbrev-mode)
  :ensure t)

(use-package eldoc
  :diminish eldoc-mode)

(use-package autorevert
  :diminish (auto-revert-mode))
