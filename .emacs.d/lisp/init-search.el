;;; init-search.el --- Search Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package isearch
  :ensure nil
  :custom
  (search-whitespace-regexp ".*\\b")
  (isearch-lax-whitespace t)
  (isearch-allow-scroll t)
  ;; TODO
  ;; (isearch-yank-on-move 'shift)
  (isearch-yank-on-move t)
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

(provide 'init-search)
;;; init-search.el ends here
