;;; evil-module.el --- settings for evil mode
;;; Commentary:
;; Configure package evil and other related packages
;;; Code:

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "tt"
    (if (or (eq system-type 'gnu/linux) (eq system-type 'darwin)
	    'vterm
	    'eshell))
    "op" 'treemacs
    "oa" 'org-agenda
    "pf" 'projectile-command-map
    "fs" 'save-buffer
    "," 'counsel-switch-buffer
    "bb" 'counsel-switch-buffer
    "kb" 'kill-buffer
    "kk" 'kill-buffer-and-window
    "qq" 'quit-window
    "ff" 'find-file
    "/" 'swiper
    "ss" 'swiper
    "ws" 'split-window-horizontally
    "wv" 'split-window-vertically
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "gs" 'magit))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "TAB") nil))

(define-key evil-motion-state-map (kbd "TAB") 'indent-for-tab-command)

(provide 'evil-module)
;;; evil-module.el ends here
