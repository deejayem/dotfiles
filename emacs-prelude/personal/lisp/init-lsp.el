(use-package lsp-mode
  :hook (clojure-mode . lsp)
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        lsp-ui-doc-delay 1
        lsp-lens-enable nil
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-symbol-highlighting t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions nil
        lsp-modeline-code-actions-enable nil ;; TODO this causes errors in *Messages* if t
        lsp-modeline-diagnostics-enable nil
        ;; user cider for indendation and completion instead
        lsp-enable-indentation nil
        lsp-completion-enable nil))

(provide 'init-lsp)

