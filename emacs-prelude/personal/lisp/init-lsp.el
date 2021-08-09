(use-package lsp-mode
  :diminish
  :after key-chord
  :hook (clojure-mode . lsp)
  :config
  (setq ;gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        lsp-ui-doc-delay 1
        lsp-lens-enable nil
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-symbol-highlighting t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-ui-doc-position 'top
        ;; user cider for indendation and completion instead
        lsp-enable-indentation nil
        lsp-completion-enable nil)
  (key-chord-define-global "QQ" 'lsp-find-references)
  (key-chord-define-global "PP" 'lsp-peek-find-references)
  (key-chord-define-global "GG" 'lsp-find-definition)
  (key-chord-define-global "DD" 'lsp-peek-find-definition))

(provide 'init-lsp)
