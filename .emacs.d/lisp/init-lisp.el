;;; init-lisp.el --- LISP Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'init-paredit)

;; From https://github.com/jwiegley/dot-emacs/blob/master/init.el
(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))

(provide 'init-lisp)
;;; init-lisp.el ends here
