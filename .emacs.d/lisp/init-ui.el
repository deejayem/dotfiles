;;; init-ui.el --- UI Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Some parts copied from prelude-ui.el and prelude-editor.el
;; {menu,tool,scoll}-bar-mode disabled in early-init.el, rather than here
;;; Code:

(use-package emacs
  :config
  (toggle-frame-maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (set-face-attribute 'default nil :font "iosevka comfy")

  ;; https://github.com/rougier/elegant-emacs/blob/master/sanity.el
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        inhibit-startup-message t
        initial-scratch-message nil)

  (blink-cursor-mode -1)

  (setq whitespace-line-column 120)
  (setq show-trailing-whitespace t)
  (setq-default indicate-empty-lines t)
  (setq ring-bell-function 'ignore
        visible-bell t)

  ;; TODO do we want these? (copied from prelude)
  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1)

  (global-display-line-numbers-mode)
  (global-hl-line-mode +1)

  (fset 'yes-or-no-p 'y-or-n-p)

  (global-set-key (kbd "C-x C-S-k") 'kill-this-buffer)

  (setq frame-title-format
        '("Emacs: " (:eval (if (buffer-file-name)
                               (abbreviate-file-name (buffer-file-name))
                             "%b"))))

  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'meta)
    (setq mac-right-option-modifier 'none)
    (setq mac-command-modifier 'super)))

(use-package modus-themes
  :init
  (setq modus-themes-syntax
        (if (member (format-time-string "%a") '("Mon" "Fri"))
            '(alt-syntax green-strings yellow-comments)
          '(green-strings-yellow-comments))
        modus-themes-paren-match '(bold intense underline)
        modus-themes-region '(accented)
        modus-themes-hl-line '(underline)
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-lang-checkers '(text-also background))
  (load-theme 'modus-vivendi t))

(use-package whitespace
  :ensure nil
  :diminish
  :custom
  (whitespace-line-column 120)
  (whitespace-style '(face tabs empty trailing lines-tail))
  :hook
  (text-mode . (lambda () (whitespace-mode +1)))
  (prog-mode . (lambda () (whitespace-mode +1)))
  (cider-repl-mode . (lambda () (whitespace-mode -1)))
  (ielm-mode . (lambda () (whitespace-mode -1)))
  (vterm-mode . (lambda () (whitespace-mode -1)))
  (eshell-mode . (lambda () (whitespace-mode -1))))

(use-package hi-lock
  :diminish
  :config
  (when (eq system-type 'darwin)
    (defun my/toggle-highlight-symbol-at-point ()
      (interactive)
      (if hi-lock-interactive-patterns
          (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
        (highlight-symbol-at-point)))
    (global-set-key (kbd "s-.") 'my/toggle-highlight-symbol-at-point)))

(use-package volatile-highlights
  :diminish
  :config
  (volatile-highlights-mode t))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package which-key
  :diminish
  :config (which-key-mode +1))

(use-package hl-todo
  :after modus-themes
  :bind
  (:map hl-todo-mode-map
        ("C-c c t p" . hl-todo-previous)
        ("C-c c t n" . hl-todo-next)
        ("C-c c t o" . hl-todo-occur)
        ("C-c c t r" . hl-todo-rgrep)
        ("C-c c t i" . hl-todo-insert))
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . "red3")
     ("djm"    . "green")
     ("FIXME"  . "red3")
     ("DEBUG"  . "#A020F0")
     ("GOTCHA" . "#FF4500")
     ("HACK" . "#FF4500")
     ("STUB"   . "#1E90FF")
     ("FAIL"   . "red3")
     ("NOTE"   . "DarkOrange2")
     ("DEPRECATED" . "yellow")))
  :init
  (global-hl-todo-mode 1))

(use-package rainbow-delimiters
  :hook
  (text-mode . (lambda () (rainbow-delimiters-mode +1)))
  (prog-mode . (lambda () (rainbow-delimiters-mode +1))))

(use-package rainbow-mode
  :diminish)

(use-package smartrep)

(use-package pulsar
  :custom
  (pulsar-pulse-on-window-change t)
  (pulsar-pulse t)
  (pulsar-iterations 80)
  (pulsar-face 'pulsar-red)
  (pulsar-pulse-functions '(recenter-top-bottom
                            move-to-window-line-top-bottom
                            reposition-window
                            forward-page
                            backward-page
                            scroll-up-command
                            scroll-down-command))
  :hook
  (isearch-update-post-hook . pulsar-pulse-line)
  (consult-after-jump-hook . pulsar-pulse-line)
  :bind ("C-c c p" . pulsar-pulse-line)
  :init
  (pulsar-global-mode 1))

(use-package lin
  :init
  (lin-global-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here
