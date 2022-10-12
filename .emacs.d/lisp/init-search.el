;;; init-search.el --- Search Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-feature isearch
  :custom
  (search-whitespace-regexp ".*\\b")
  (isearch-lax-whitespace t)
  (isearch-allow-scroll t)
  (isearch-yank-on-move 'shift)
  (isearch-lazy-count t)
  :bind-keymap ("C-c s" . search-map) ;; M-s clashes with paredit/smartparens bindings
  :bind
  ("C-*" . isearch-forward-symbol-at-point)
  (:map search-map
   ("M-s M-<" . isearch-beginning-of-buffer)
   ("M-s M->" . isearch-end-of-buffer)
   ("C-c s M-<" . isearch-beginning-of-buffer)
   ("C-c s M->" . isearch-end-of-buffer)))

(use-package isearch-dabbrev
  :after isearch
  :bind (:map isearch-mode-map ("M-/" . isearch-dabbrev-expand)))

(use-package anzu
  :diminish
  :config
  (global-anzu-mode)
  (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold)
  :custom
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-replace-threshold 100)
  (anzu-replace-to-string-separator " => ")
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp)
  (:map isearch-mode-map
        ([remap isearch-query-replace] . anzu-isearch-query-replace)
        ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

(use-package rg
  :bind
  ("C-c C-M-S-r" . rg-menu)
  ("C-c C-M-r" . rg))

(use-package deadgrep)

(use-package affe
  :after (consult orderless)
  :config
  (setq affe-grep-command (replace-regexp-in-string "\\." "-Suu ." affe-grep-command))
  ;; Configure Orderless
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
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
  :bind
  ("C-#" . affe-grep)
  ("C-c z" . affe-find)
  ("C-c Z" . my/affe-find-symbol-at-point)
  ("C-~" . my/affe-grep-symbol-at-point))

(provide 'init-search)
;;; init-search.el ends here
