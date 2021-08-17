;;; init-lsp.el --- LSP Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-ui
  :config
  (require 'lsp-ui-imenu))

(use-package lsp-mode
  :diminish
  :after key-chord
  :hook (clojure-mode . lsp)
  :config
  (if (eq system-type 'darwin)
      (setq lsp-keymap-prefix "s-l")
    (setq lsp-keymap-prefix "C-c C-l"))
  (setq read-process-output-max (* 1024 1024)
        lsp-ui-sideline-enable t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
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
        lsp-treemacs-theme "Iconless"
        ;; user cider for indendation and completion instead
        lsp-enable-indentation nil
        lsp-completion-enable nil)
  (key-chord-define-global "QQ" 'lsp-find-references)
  (key-chord-define-global "PP" 'lsp-peek-find-references)
  (key-chord-define-global "GG" 'lsp-find-definition)
  (key-chord-define-global "DD" 'lsp-peek-find-definitions)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)))

(use-package lsp-treemacs)

(provide 'init-lsp)
;;; init-lsp.el ends here
