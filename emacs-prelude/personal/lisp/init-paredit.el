(defun add-hooks (modes func)
 (dolist (mode modes)
  (add-hook (intern (concat (symbol-name mode) "-hook")) func)))

(setq lisp-modes
 '(scheme-mode emacs-lisp-mode lisp-mode clojure-mode cider-repl-mode
   eval-expression-minibuffer-setup ielm-mode lisp-interaction-mode))

(defun add-lisp-hook (func)
  (add-hooks lisp-modes func))

(prelude-require-package 'paredit)
(use-package paredit
  :diminish
  :bind (([remap mark-sexp] . sp-mark-sexp) ;; sp does this better!
         ("M-[" . paredit-wrap-square))
  :config
  (add-lisp-hook #'turn-off-smartparens-mode)
  (add-lisp-hook #'enable-paredit-mode))

(use-package paredit-functions
  :after paredit
  :ensure nil
  :load-path "~/.emacs.d/personal/lisp/paredit-functions.el")

(provide 'init-paredit)

