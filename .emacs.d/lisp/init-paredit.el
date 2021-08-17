;;; init-paredit.el --- Paredit Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; add-hooks/add-lisp-hook based on https://github.com/bodil/emacs.d/blob/master/bodil/bodil-lisp.el
;;; Code:

(defun add-hooks (modes func)
 (dolist (mode modes)
  (add-hook (intern (concat (symbol-name mode) "-hook")) func)))

(setq lisp-modes
 '(scheme-mode emacs-lisp-mode lisp-mode clojure-mode cider-repl-mode
   eval-expression-minibuffer-setup ielm-mode lisp-interaction-mode))

(defun add-lisp-hook (func)
  (add-hooks lisp-modes func))

(use-package paredit
  :diminish
  ;; sp does a few things better
  :bind (([remap mark-sexp] . sp-mark-sexp)
         ("M-[" . sp-wrap-square)
         ("C-c M-{" . sp-wrap-curly)
         ([remap paredit-wrap-round] . sp-wrap-round)
         ([remap paredit-meta-doublequote] . sp-wrap-double-quotation-marks)
         ("M-W" . paredit-copy-as-kill))
  :config
  (defun sp-wrap-double-quotation-marks ()
    (interactive)
    (sp-wrap-with-pair "\""))
  :init
  (add-lisp-hook #'turn-off-smartparens-mode)
  (add-lisp-hook #'enable-paredit-mode))

(provide 'init-paredit)
;;; init-paredit.el ends here
