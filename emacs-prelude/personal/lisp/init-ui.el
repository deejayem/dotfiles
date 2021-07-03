(toggle-frame-maximized)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(prelude-require-package 'solarized-theme)
(load-theme 'solarized-dark t)
;(set-face-background 'hi-yellow "yellow")

(setq whitespace-line-column 120)

(require 'hi-lock)
(defun toggle-highlight-symbol-at-point ()
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))
(global-set-key (kbd "s-.") 'toggle-highlight-symbol-at-point)

(use-package paren
  :config
  (show-paren-mode +1))

(global-set-key (kbd "C-x C-S-k") 'kill-this-buffer)

(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(provide 'init-ui)

