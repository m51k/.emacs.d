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

(use-package delight
  :ensure (:wait t)
  :demand t)

(use-package emacs
  :ensure nil
  :delight (eldoc-mode abbrev-mode)
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;; (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-auto-unbind-keys)
  (general-create-definer global-leader
    :prefix "C-c")
  (global-leader
    "t"  'eshell
    "ec" 'flymake-start
    "s"  'consult-line
    "es" 'consult-flymake))

;; (use-package evil
;;   :demand t
;;   :init
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-integration t)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-C-i-jump nil)
;;   :config
;;   (evil-mode 1)
;;   (dolist (mode '(eshell-mode
;; 		  dired-mode
;; 		  magit-mode
;; 		  text-mode))
;;     (add-to-list 'evil-emacs-state-modes mode))
;;   (evil-set-initial-state 'org-mode 'normal)
;;   :delight evil-mode)

(use-package which-key
  :init (which-key-mode)
  :delight which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package vertico
  :demand t
  :init
  (vertico-mode)
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
  :general
  (global-leader
   "b" 'consult-buffer))

(use-package embark
  :demand t
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :demand t)

(use-package corfu
  :demand t
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

(use-package transient
  :demand t)

(use-package magit
  :after general
  :demand t
  :general
  (global-leader
    "gs" 'magit))

(use-package project
  :ensure nil)

(use-package consult-project-extra
  :after general
  :demand t
  :general
  (global-leader
    "pf" 'consult-project-extra-find
    "po" 'consult-project-extra-find-other-window))

(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            ))
(electric-pair-mode t)

;; (use-package lsp-mode
;;   :demand t
;;   :custom
;;   (lsp-completion-provider :none) 
;;   :init
;;   ;; use orderless
;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless)))
;;   :hook
;;   (lsp-completion-mode . my/lsp-mode-setup-completion)
;;   ((php-mode blade-mode robot-mode) . lsp-mode)
;;   :config
;;   ;; register languages
;;   (add-to-list 'lsp-language-id-configuration '((php-mode blade-mode) . "php"))
;;   (add-to-list 'lsp-language-id-configuration '(robot-mode . "robot"))
;;   ;; configure clients
;;   (lsp-register-client (make-lsp-client
;;     			:new-connection (lsp-stdio-connection "emacs-lsp-booster -q -- intelephense --stdio")
;;     			:activation-fn (lsp-activate-on "php")
;;     			:server-id 'iph))
;;   (lsp-register-client (make-lsp-client
;; 			:new-connection (lsp-stdio-connection "robotframework_ls")
;; 			:activation-fn (lsp-activate-on "robot")
;; 			:server-id 'robotframework-lsp)))

(use-package eglot
  :ensure nil
  :demand t
  :hook
  ((php-mode blade-mode robot-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '(((php-mode) (blade-mode :language-id "php")) . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
	       '(robot-mode . ("robotframework_ls"))))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :demand t
  :config (eglot-booster-mode))

(use-package eldoc-box
  :demand t
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  :general
  (general-define-key
   "C-h ." 'eldoc-box-help-at-point))

(use-package web-mode
  :demand t
  :mode ("\\.html?\\'" "\\.css?\\'" "\\.js?\\'")
  :config
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing t)
  (define-derived-mode blade-mode web-mode "Blade")
  (setq auto-mode-alist
	(append '(("\\.blade\\.php\\'" . blade-mode))
		auto-mode-alist)))

(use-package php-mode
  :demand t)

(use-package robot-mode
  :demand t)

(use-package prettier
  :demand t
  :hook (web-mode . prettier-mode))

(use-package tree-sitter
  :demand t
  :delight
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :demand t
  :after tree-sitter)

(use-package org
  :ensure (:wait t)
  :demand t
  :delight auto-revert-mode
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-agenda-files '("~/Org/agenda.org"))
  :general
  (global-leader
    "a" 'org-agenda))

(use-package org-roam
  :ensure (:depth 1)
  :demand t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Org")
  :config
  (org-roam-setup)
  :general
  (global-leader
    "nl" 'org-roam-buffer-toggle
    "nf" 'org-roam-node-find
    "ni" 'org-roam-node-insert))

(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  :hook (org-roam-mode . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; (use-package evil-org
;;   :after org
;;   :demand t
;;   :commands evil-org-mode
;;   :delight evil-org-mode
;;   :init
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   :config
;;   (add-hook 'evil-org-mode-hook
;; 	    (lambda ()
;; 	      (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading)))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq mode-line-position (list "(%l:%C) %p"))
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
                "  "
                mode-line-modes
                vc-mode
		" "
                mode-line-misc-info
                mode-line-end-spaces))

(use-package moe-theme
  :demand t
  :config
  (load-theme 'moe-dark :no-confirm))
