;;; core-editor.el --- general editor settings
;;; Commentary:
;; General editor settings
;;; Code:


;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(defun mk/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'mk/display-startup-time)

(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message 1)
(setq make-backup-files nil) ; stop creating ~ files
(setq create-lockfiles nil)

(scroll-bar-mode -1)        ;; Disable scrollbar
(tool-bar-mode -1)          ;; Disable toolbar
(tooltip-mode -1)           ;; Disable tooltips
(set-fringe-mode 10)        ;; idk
(menu-bar-mode -1)          ;; Disable menu bar
(global-hl-line-mode)

(set-face-attribute 'default nil :font "Iosevka Comfy" :height 100)

(column-number-mode)
(global-display-line-numbers-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(provide 'core-editor)
;;; core-editor.el ends here
