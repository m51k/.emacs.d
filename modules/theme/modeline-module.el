;;; modeline-module.el --- settings for package doom-modeline
;;; Commentary:
;; Configure package doom-modeline
;;; Code:

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; Left.
     (quote ("%e "
             mode-line-buffer-identification
             " %l : %c"
             evil-mode-line-tag
             "[%*]"))
     ;; Right.
     (quote ("%p "
             mode-line-frame-identification
             mode-line-modes
             mode-line-misc-info))))))

(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(provide 'modeline-module)
;;; modeline-module.el ends here
