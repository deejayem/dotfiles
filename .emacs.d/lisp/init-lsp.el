;;; init-lsp.el --- LSP Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-doc-delay 1
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-doc-position 'at-point))

(use-package lsp-treemacs)

(use-package lsp-mode
  :diminish
  :hook
  (clojure-mode . lsp)
  (lsp-lens-mode . really-diminish-lsp-lens-mode)
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (sql-mode . lsp)
  (lsp-after-apply-edits . save-buffer)
  :config
  (defun really-diminish-lsp-lens-mode ()
    (diminish 'lsp-lens-mode)
    (remove-hook 'lsp-lens-mode-hook 'really-diminish-lsp-lens-mode))
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Remove this, as we use cape-clojure (in init-clojure.el), which includes
    ;; lsp-completion-at-point
    (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t))
  (if (eq system-type 'darwin)
      (setq lsp-keymap-prefix "s-l")
    (setq lsp-keymap-prefix "C-c C-l"))
  (define-key lsp-mode-map (kbd lsp-keymap-prefix) lsp-command-map)
  (setq lsp-sqls-workspace-config-path nil)
  (setq read-process-output-max (* 1024 1024)
        lsp-log-io nil
        lsp-lens-enable t
        lsp-headerline-breadcrumb-enable t
        lsp-idle-delay 1.000
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-treemacs-theme "Iconless"
        lsp-completion-provider :none ;; use corfu
        lsp-references-exclude-definition t
        ;; user cider for indendation and eldoc
        lsp-enable-indentation nil
        lsp-eldoc-enable-hover nil))

(provide 'init-lsp)
;;; init-lsp.el ends here
