;;; general-module.el --- settings for evil mode
;;; Commentary:
;; Configure package general and global keybinds
;;; Code:

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer leader-keys
			  :states '(normal visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (leader-keys
    "tt"
    (if (or (eq system-type 'gnu/linux) (eq system-type 'darwin))
        'vterm
      'eshell)
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
    "op" 'treemacs
    "/" 'swiper
    "," 'counsel-switch-buffer
    "pf" 'projectile-command-map))

(general-auto-unbind-keys)

(provide 'general-module)
;;; general-module.el ends here
