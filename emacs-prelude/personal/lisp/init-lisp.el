(require 'init-paredit)

(prelude-require-package 'eval-expr)
(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'completion-at-point)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))

(provide 'init-lisp)

