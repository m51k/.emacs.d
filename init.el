(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
  			    :ref nil :depth 1
  			    :files (:defaults "elpaca-test.el" (:exclude "extensions"))
  			    :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
  	       ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
  					       ,@(when-let ((depth (plist-get order :depth)))
  						   (list (format "--depth=%d" depth) "--no-single-branch"))
  					       ,(plist-get order :repo) ,repo))))
  	       ((zerop (call-process "git" nil buffer t "checkout"
  				     (or (plist-get order :ref) "--"))))
  	       (emacs (concat invocation-directory invocation-name))
  	       ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
  				     "--eval" "(byte-recompile-directory \".\" 0 'force)")))
  	       ((require 'elpaca))
  	       ((elpaca-generate-autoloads "elpaca" repo)))
  	  (progn (message "%s" (buffer-string)) (kill-buffer buffer))
  	(error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; (require 'package)

;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; ;; Initialize use-package for non-linux
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)

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
  :demand t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-auto-unbind-keys)
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
    "cc" 'comment-line
    "cr" 'comment-or-uncomment-region
    "op" 'dired-jump))

(elpaca-wait)

(use-package evil
  :demand t
  :config
  (evil-mode 1)
  (leader-keys
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right)
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
  :demand t
  :init
  (vertico-mode)
  :bind (:map vertico-map
  	    ("C-j" . vertico-next)
  	    ("C-k" . vertico-previous))
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil))

(savehist-mode 1)

(use-package marginalia
  :after vertico
  :demand t
  :config
  (marginalia-mode 1))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :demand t
  :config
  (setq consult-narrow-key "<")
  (leader-keys
    "s" 'consult-line
    "b" 'consult-buffer))

(use-package embark
  :demand t
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :demand t)

(use-package corfu
  :demand t
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

(use-package transient
  :demand t)

(use-package magit
  :after general
  :demand t
  :diminish magit-auto-revert-mode
  :config
  (leader-keys
    "gs" 'magit))

(require 'project)

(use-package consult-project-extra
  :after general
  :demand t
  :config
  (leader-keys
    "pf" 'consult-project-extra-find
    "po" 'consult-project-extra-find-other-window))

(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            ))
(electric-pair-mode t)

(use-package lsp-mode
  :demand t
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
  :demand t
  :mode ("\\.html?\\'" "\\.css?\\'" "\\.js?\\'" "\\.blade\\.php\\'")
  :config
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing t))

(use-package php-mode
  :demand t)

(use-package robot-mode
  :demand t)

(use-package prettier
  :demand t
  :hook ((web-mode . prettier-mode)))

(use-package flycheck
  :demand t
  :diminish flycheck-mode
  :init (global-flycheck-mode))

(use-package tree-sitter
  :demand t
  :diminish tree-sitter-mode
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :demand t
  :after tree-sitter)

(use-package org
  :ensure (:wait t)
  :demand t
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-agenda-files '("~/Org/agenda.org"))
  (leader-keys
    "oal" 'org-agenda-list
    "ots" 'org-time-stamp))

(use-package org-roam
  :demand t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Org")
  :config
  (org-roam-setup)
  (leader-keys
    "nl" 'org-roam-buffer-toggle
    "nf" 'org-roam-node-find
    "ni" 'org-roam-node-insert))

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
  :after org
  :demand t
  :commands evil-org-mode
  :diminish evil-org-mode
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
  	  (lambda ()
  	    (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading)))))

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
  :demand t
  :config
  (load-theme 'moe-dark :no-confirm))

(use-package diminish
  :demand t)
