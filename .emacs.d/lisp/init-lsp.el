;;; init-lsp.el --- LSP Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-ui
  :config
  (require 'lsp-ui-imenu))

(use-package lsp-treemacs)

(use-package lsp-mode
  :diminish
  :hook
  (clojure-mode . lsp)
  (lsp-lens-mode . really-diminish-lsp-lens-mode)
  :config
  (defun really-diminish-lsp-lens-mode ()
    (diminish 'lsp-lens-mode)
    (remove-hook 'lsp-lens-mode-hook 'really-diminish-lsp-lens-mode))
  (if (eq system-type 'darwin)
      (setq lsp-keymap-prefix "s-l")
    (setq lsp-keymap-prefix "C-c C-l"))
  (define-key lsp-mode-map (kbd lsp-keymap-prefix) lsp-command-map)
  (setq read-process-output-max (* 1024 1024)
        lsp-ui-sideline-enable t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-doc-delay 1
        lsp-lens-enable t
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-headerline-breadcrumb-enable t
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-ui-doc-position 'top
        lsp-treemacs-theme "Iconless"
        ;; user cider for indendation and completion instead
        lsp-enable-indentation nil
        lsp-completion-enable nil))

(provide 'init-lsp)
;;; init-lsp.el ends here
