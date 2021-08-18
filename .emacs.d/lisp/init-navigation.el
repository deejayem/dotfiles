;;; init-navigation.el --- Navigation Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ctrlf
  :init
  (ctrlf-mode +1)
  :config
  (add-to-list 'ctrlf-minibuffer-bindings '("C-M-g" . ctrlf-cancel))
  (add-to-list 'ctrlf-minibuffer-bindings '("C-o o" . ctrlf-occur))
  (add-to-list 'ctrlf-minibuffer-bindings '("C-o C-o" . ctrlf-occur))
  :custom
  (ctrlf-default-search-style 'fuzzy-regexp)
  (ctrlf-alternate-search-style 'literal)
  :bind
  ("C-M-g" . ctrlf-cancel) ;; always bind this in case we have left the minibuffer
  ("C-*" . ctrlf-forward-symbol-at-point))

(use-package smartscan
  :config
  (global-smartscan-mode t)
  :hook
  (cider-repl-mode . (lambda () (smartscan-mode -1)))
  (ielm-mode . (lambda () (smartscan-mode -1)))
  (vterm-mode . (lambda () (smartscan-mode -1)))
  (eshell-mode . (lambda () (smartscan-mode -1))))

(use-package affe
  :after (consult orderless)
  :config
  (setq affe-grep-command (replace-regexp-in-string "\\." "-Suu ." affe-grep-command))
  (when (and (eq system-type 'darwin) (string-match-p "^find" affe-find-command))
    (setq affe-find-command (concat "g" affe-find-command)))
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))
  (defun my/affe-grep-symbol-at-point (&optional dir initial)
    (interactive
      (list prefix-arg (when-let ((s (symbol-at-point)))
                         (symbol-name s))))
    (affe-grep dir initial))
  (defun my/affe-find-symbol-at-point (&optional dir initial)
    (interactive
      (list prefix-arg (when-let ((s (symbol-at-point)))
                         (symbol-name s))))
    (affe-find dir initial))
  :custom
  (affe-find-command "fd --color never -t f")
  :bind
  ("C-#" . affe-grep)
  ("C-c z" . affe-find)
  ("C-c Z" . my/affe-find-symbol-at-point)
  ("C-~" . my/affe-grep-symbol-at-point))

;; TODO - which of these are useful?
(use-package avy
  :after key-chord
  :custom
  (avy-background t)
  (avy-style 'pre)
  :config
  (key-chord-define-global "LL" 'avy-goto-line)
  (key-chord-define-global ",," 'avy-goto-char-in-line)
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jk" 'avy-goto-char)
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("C-c C-v" . avy-goto-char-in-line)
  ("C-c v" . avy-goto-word-or-subword-1)
  ("M-g w" . avy-goto-word-1)
  ("M-g M-w" . avy-goto-word-0)
  ("M-g M-f" . avy-goto-line))

(use-package rg
  :config
  (rg-enable-default-bindings))

(provide 'init-navigation)
;;; init-navigation.el ends here
