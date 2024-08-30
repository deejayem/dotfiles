;;; init-shell.el --- eshell/vterm Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-feature eshell
  :bind ("C-x m " . eshell)
  :hook
  (eshell-pre-command . eshell-save-some-history)
  (eshell-mode-hook . (lambda () (setenv "TERM" "xterm-256color")))
  :custom
  (eshell-directory-name (expand-file-name "eshell" save-dir))
  (eshell-prompt-function
   ;; Based on https://www.reddit.com/r/emacs/comments/6f0rkz/my_fancy_eshell_prompt/
   (lambda ()
     (concat
      (propertize "┌─[" 'face `(:foreground "green"))
      (propertize (user-login-name) 'face `(:foreground "red"))
      (propertize "@" 'face `(:foreground "green"))
      (propertize (system-name) 'face `(:foreground "LightBlue"))
      (propertize "]──[" 'face `(:foreground "green"))
      (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
      (propertize "]──[" 'face `(:foreground "green"))
      (propertize (concat (eshell/pwd)) 'face `(:foreground "white"))
      (propertize "]\n" 'face `(:foreground "green"))
      (propertize "└─>" 'face `(:foreground "green"))
      (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "green")))))
  :config
  (setenv "PAGER" "cat"))

(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package xterm-color
  :after esh-mode
  :hook
  (eshell-before-prompt . (lambda ()
                            (setq xterm-color-preserve-properties t)))
  :config
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)
  (setenv "TERM" "xterm-256color"))

;; Installed with home-manager
(use-feature multi-vterm
  :bind (("C-c t" . multi-vterm-next)
         ("C-x p t" . multi-vterm-project)
         ("C-c C-M-t" . multi-vterm)
         (:map vterm-mode-map
               ("M-[" . multi-vterm-prev)
               ("M-]" . multi-vterm-next))))

(provide 'init-shell)
;;; init-shell.el ends here
