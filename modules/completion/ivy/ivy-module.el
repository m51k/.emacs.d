;;; ivy-module.el --- settings for package ivy
;;; Commentary:
;; Configure package ivy
;;; Code:

(use-package swiper :ensure t)
(use-package ivy
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-p" . ivy-previous-line)
         ("TAB" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :config
  (counsel-mode 1))

(provide 'ivy-module)
;;; ivy-module.el ends here
