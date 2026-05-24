;;; init.el --- Main Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Emacs config
;; TODO: vibecoded slop, clean this later
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1000 1000))
            (setq gc-cons-percentage 0.5)))

(setq package-archives '(("gnu"      . "https://elpa.gnu.org/packages/")
                         ("melpa"    . "https://melpa.org/packages/")
                         ("org"      . "https://orgmode.org/elpa/")
                         ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

(setq jit-lock-defer-time 0.05)
(setq jit-lock-stealth-time 1)
(setq jit-lock-stealth-nice 0.5)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

(setq frame-resize-pixelwise nil)

(setq inhibit-compacting-font-caches t)

(setq comint-buffer-maximum-size 2000)
(add-hook 'comint-output-filter-functions #'comint-truncate-buffer)

(setq message-log-max 500)

(defun mk/profile-start ()
  "Start profiling. Run the slow interaction, then call mk/profile-stop."
  (interactive)
  (profiler-start 'cpu+mem)
  (message "Profiler started — reproduce the stutter, then M-x mk/profile-stop"))

(defun mk/profile-stop ()
  "Stop profiling and open the report."
  (interactive)
  (profiler-stop)
  (profiler-report))

(use-package no-littering
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(set-fringe-mode 0)

(setq visible-bell t)

(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode)

(defvar mk/font-size 120)

(set-face-attribute 'default       nil :font "Iosevka" :height mk/font-size)
(set-face-attribute 'fixed-pitch   nil :inherit 'default)
(set-face-attribute 'variable-pitch nil :inherit 'default)

(setq mode-line-position (list "(%l:%C) %p of %I "))
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client-mode
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
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

(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                ;; vterm-mode-hook
		))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(blink-cursor-mode -1)
(setq cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(use-package so-long
  :hook (after-init . global-so-long-mode)
  :config
  (setq so-long-threshold 1000)
  (setq so-long-max-lines 100))

(use-package general
  :custom
  (general-auto-unbind-keys t)
  :hook (after-init . general-override-mode))

(general-create-definer cc-leader :prefix "C-c")
(general-create-definer cx-leader :prefix "C-x")

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1))

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 15)
  (vertico-preselect 'first)
  (vertico-scroll-margin 0)
  :config
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :general
  (:keymaps 'vertico-map
   "RET"   'vertico-directory-enter
   "DEL"   'vertico-directory-delete-char
   "M-DEL" 'vertico-directory-delete-word))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-annotators '(marginalia-annotators-light marginalia-annotators-heavy nil)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file     (styles basic partial-completion))
     (command  (styles orderless))
     (symbol   (styles orderless))
     (variable (styles orderless)))))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (consult-project-root-function #'projectile-project-root)
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 3)
  (consult-async-refresh-delay 0.2)
  (consult-async-input-throttle 0.4)
  (consult-async-input-debounce 0.3)
  :general
  ("C-s"   'consult-line)
  ("C-r"   'consult-line)
  ("M-#"   'consult-register-load)
  ("M-'"   'consult-register-store)
  ("C-M-#" 'consult-register)
  (cx-leader
    "C-f" 'find-file
    "b"   'consult-buffer
    "r"   'consult-recent-file
    "y"   'consult-yank-pop
    "i"   'consult-imenu
    ;; "o"   'consult-outline
    "s"   'consult-ripgrep
    "gl"  'consult-git-log))

(use-package projectile
  :hook (after-init . projectile-mode)
  :custom
  (projectile-completion-system 'default)
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  (projectile-generic-command "rg --files --hidden --glob '!.git'"))

(use-package consult-projectile
  :after projectile
  :general
  (cx-leader
    "pp" 'consult-projectile-switch-project
    "pf" 'consult-projectile-find-file
    "pd" 'consult-projectile-find-dir
    "pg" 'consult-git-grep
    "ps" 'consult-ripgrep))

(use-package embark
  :general
  (cc-leader "." 'embark-act)
  ("C-." 'embark-act)
  ("M-." 'embark-dwim)
  :custom
  (embark-quit-after-action nil)
  (embark-indicators '(embark-highlight-indicator embark-isearch-highlight-indicator))
  (embark-prompter 'embark-completing-read-prompter))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 3)
  (corfu-popupinfo-delay 0.8)
  (corfu-popupinfo-max-height 15)
  :config
  (corfu-popupinfo-mode 1)
  :general
  (:keymaps 'corfu-map
   "C-n"   'corfu-next
   "C-p"   'corfu-previous
   "C-j"   'corfu-complete
   "TAB"   'corfu-complete-common-or-cycle
   "S-TAB" 'corfu-previous
   "RET"   'corfu-insert))

(use-package cape
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(add-to-list 'exec-path (expand-file-name "~/.emacs.d/var/mason/bin"))

(use-package lsp-mode
  :hook ((LaTeX-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-enable-suggest-server-download t)
  (lsp-auto-guess-root t)
  (lsp-enable-snippet t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)

  (lsp-inlay-hints-show-parameter-names t)
  (lsp-inlay-hints-show-parameter-type t)
  (lsp-inlay-hints-show-variable-types t)
  (lsp-inlay-hints-show-function-return-type t)

  (lsp-lens-enable nil)

  (lsp-completion-provider :capf)
  (lsp-diagnostics-provider :flycheck)
  (lsp-disabled-clients '(digestif))

  (lsp-enable-file-watchers nil)

  (lsp-log-io nil)
  (lsp-idle-delay 0.8)

  (lsp-before-save-edits nil)
  (lsp-completion-enable-additional-text-edit t)

  (lsp-eldoc-enable-hover nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-log-max 1000)
  (lsp-keep-workspace-alive nil)
  :general
  (cc-leader
    "r n" 'lsp-rename
    "c a" 'lsp-execute-code-action)
  :config
  (set-face-attribute 'lsp-inlay-hint-face nil
		      :height 0.85
		      :slant 'italic
		      :foreground "#5c5c5c"
		      :background "#f2f2f2"
		      :box '(:line-width -1
					 :color "#d4d4d4"
					 :style nil)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; Hover docs
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.8)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-alignment 'window)

  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-delay 0.8)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-ignore-duplicate t)

  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)

  (lsp-ui-imenu-enable nil)
  :general
  (:keymaps 'lsp-ui-mode-map
   [remap xref-find-definitions] 'lsp-ui-peek-find-definitions
   [remap xref-find-references]  'lsp-ui-peek-find-references))

(use-package consult-lsp
  :after (consult lsp-mode)
  :general
  (cc-leader
    "l"   'consult-lsp-symbols
    "e n" 'consult-lsp-diagnostics))

(use-package mason
  :config (mason-setup)
  :general
  (cc-leader
    "m i" 'mason-install
    "m m" 'mason-manager
    "m d" 'mason-doctor))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yas-reload-all))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode)
	 (LaTeX-mode . flycheck-mode))
  :custom
  (flycheck-display-errors-delay 0.9)
  (flycheck-idle-change-delay 1.5)
  (flycheck-indication-mode 'right-fringe))

(use-package consult-flycheck
  :after (consult flycheck)
  :general
  (cc-leader "el" 'consult-flycheck))

(use-package magit
  :general
  (cc-leader
    "gs" 'magit-status
    "gb" 'magit-blame
    "gl" 'magit-log))

(use-package diff-hl
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (prog-mode          . turn-on-diff-hl-mode)
         (vc-dir-mode        . turn-on-diff-hl-mode))
  :custom
  (diff-hl-side 'left))

(setq-default TeX-master nil)

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-complete-graphical t)
  (font-latex-fontify-sectioning 1)
  (TeX-parse-self t)
  (TeX-auto-save t))

(use-package citar
  :after tex
  :custom
  (citar-bibliography '("~/School/FYP/Final/references.bib"))
  (citar-library-paths '("~/School/FYP/Final/Papers/"))
  (citar-notes-paths '("~/School/FYP/Final/Notes/"))
  (citar-select-multiple t)
  :hook (LaTeX-mode . citar-capf-setup)
  :general
  (cc-leader :keymaps 'LaTeX-mode-map
    "b" 'citar-insert-citation
    "o" 'citar-open-library-files))

(use-package citar-embark
  :after (citar embark)
  :config (citar-embark-mode))

(use-package lsp-ltex-plus
  :after lsp-mode
  :custom
  (lsp-ltex-plus-auto-download nil)
  (lsp-ltex-plus-version "18.6.1")
  (lsp-ltex-plus-language "en-GB")
  (lsp-ltex-plus-diagnostic-severity "information")
  :hook
  (LaTeX-mode . (lambda ()
                  (lsp-deferred)
                  (setq lsp-enabled-clients '(ltex-ls-plus texlab)))))

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist
   `(("." . ,(no-littering-expand-var-file-name "undo-tree/"))))
  :general
  (cc-leader "u" 'undo-tree-visualize))

(use-package super-save
  :hook (after-init . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 5))

(use-package zoom
  :custom (zoom-size '(0.618 . 0.618))
  :general
  (cc-leader "wz" 'zoom))

(use-package moe-theme
  :hook (after-init . (lambda () (load-theme 'moe-light t))))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package all-the-icons
  :defer t)

;;; init.el ends here
