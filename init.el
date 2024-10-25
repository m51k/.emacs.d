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
		"%-"
                mode-line-misc-info
                mode-line-end-spaces))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-auto-unbind-keys)

  (general-create-definer global-leader
    :prefix "C-c")
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
	org-agenda-files '("~/Documents/Notes/agenda.org")
	org-capture-templates
	'(("f" "Fleeting" entry (file "~/Documents/Notes/inbox.org")
           "* %?\n")))
  (defun m51k/org-capture-fleeting ()
    (interactive)
    (org-capture nil "f"))
  :general
  (global-leader
    "oa" 'consult-org-agenda
    "oh" 'consult-org-heading
    "oc" 'org-capture
    "of" 'm51k/org-capture-fleeting))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-directory "~/Documents/Notes/")
  (org-roam-capture-templates
   '(("m" "main"
      plain (file "~/Documents/Notes/main/main-template.org")
      :if-new (file+head "main/${slug}.org"
			 "#+title: ${title}\n")
      :unnarrowed t)
     ("r" "reference"
      plain (file "~/Documents/Notes/reference/reference-template.org")
      :if-new (file+head "reference/${title}.org"
			 "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-node-display-template
   (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))
  (org-roam-setup)
  :general
  (global-leader
   "nl" 'org-roam-buffer-toggle
   "ni" 'org-roam-node-insert))

(use-package consult-org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-.")
   :general
   (global-leader
     "nf" 'consult-org-roam-file-find
     "nb" 'consult-org-roam-backlinks
     "nB" 'consult-org-roam-backlinks-recursive
     "nl" 'consult-org-roam-forwards-links
     "ns" 'consult-org-roam-search))

(use-package org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

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

(use-package moe-theme
  :config
  (load-theme 'moe-dark :no-confirm))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult-project-extra magit org-roam general which-key vertico orderless no-littering moe-theme marginalia embark-consult corfu auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
