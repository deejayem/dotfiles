;;; init-lisp.el --- LISP Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'init-paredit)

(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'completion-at-point)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))

(provide 'init-lisp)
;;; init-lisp.el ends here
