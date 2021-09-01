;;; init-search.el --- Search Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package isearch
  :ensure nil
  :custom
  (search-whitespace-regexp ".*\\b")
  (isearch-lax-whitespace t)
  (isearch-allow-scroll t)
  (isearch-yank-on-move 'shift)
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

(provide 'init-search)
;;; init-search.el ends here
