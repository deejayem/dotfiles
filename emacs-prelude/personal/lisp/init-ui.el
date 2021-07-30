(toggle-frame-maximized)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(prelude-require-package 'solarized-theme)
(load-theme 'solarized-dark t)

(setq whitespace-line-column 120)

(require 'hi-lock)
(defun my/toggle-highlight-symbol-at-point ()
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))
(global-set-key (kbd "s-.") 'my/toggle-highlight-symbol-at-point)

(unless (eq system-type 'darwin)
  (prelude-require-package 'idle-highlight-mode)
  (use-package idle-highlight-mode
    :hook (prog-mode . idle-highlight-mode)))

(use-package paren
  :config
  (show-paren-mode +1))

(global-set-key (kbd "C-x C-S-k") 'kill-this-buffer)

(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(prelude-require-package 'ctrlf)
(use-package ctrlf
  :init
  (ctrlf-mode +1)
  :bind
  ("C-*" . ctrlf-forward-symbol-at-point)
  ("M-s M-s" . ctrlf-toggle-symbol)
  ("C-M-g" . ctrlf-cancel)
  ("C-c o" . ctrlf-occur))

(prelude-require-package 'buffer-move)
(use-package buffer-move
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(prelude-require-package 'smartscan)
(use-package smartscan
  :config
  (global-smartscan-mode t))

(prelude-require-package 'fullframe)
 (use-package fullframe
  :after magit
   :config
   (fullframe magit-status magit-mode-quit-window))

(provide 'init-ui)
