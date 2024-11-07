;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

(setq inhibit-startup-message 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)
(menu-bar-mode -1)

(set-face-attribute 'default nil :font "Jetbrains Mono" :height 100)

(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode)

(use-package no-littering)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(setq mode-line-position (list "(%l:%C) %p of %I "))
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
		mode-line-position
                " "
                mode-line-modes
                vc-mode
		" "
                mode-line-misc-info
                mode-line-end-spaces))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :after evil
  :config
  (general-auto-unbind-keys)
  (general-create-definer global-leader
    :keymaps '(normal insert visual emacs)
    :prefix "C-c"
    :global-prefix "C-c")
  (global-leader
    "t"  'eshell))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t
	vertico-resize nil))

(savehist-mode 1)

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :config
  (setq consult-narrow-key "?")
  :general
  (global-leader
   "b" 'consult-buffer))

(use-package embark
  :bind (("C-." . embark-act)))

(use-package embark-consult)

(use-package corfu
  :bind (:map corfu-map
	      ("TAB" . corfu-insert)
	      ([tab] . corfu-insert)
	      ("RET" . nil))
  :custom
  (completion-category-overrides '((eglot (styles orderless))))
  (completion-category-defaults nil)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  :config
  (global-corfu-mode))

(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            ))
(electric-pair-mode t)

(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-agenda-files (directory-files-recursively "~/notes/" "\\.org$")
	org-capture-templates
	'(("r" "Random" entry (file "~/notes/inbox.org")
           "* %?\n")))
  :general
  (global-leader
    "os" 'consult-org-agenda
    "oa" 'org-agenda
    "oh" 'consult-org-heading
    "oc" 'org-capture))

(use-package magit
  :general
  (global-leader
    "gs" 'magit))

(use-package project)

(use-package consult-project-extra
  :general
  (global-leader
    "pf" 'consult-project-extra-find
    "po" 'consult-project-extra-find-other-window))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-owl t))
