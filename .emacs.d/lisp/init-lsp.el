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
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("coffeesense-language-server" "--stdio"))
    :major-modes '(coffee-mode)
    :server-id 'coffeesense))

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
        lsp-eldoc-enable-hover nil)

  ;; Copied from https://github.com/blahgeek/emacs-lsp-booster/blob/4200ed6ae0cd83b8e3fd1dbefb09121480951a22/README.md#configure-lsp-mode
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(provide 'init-lsp)
;;; init-lsp.el ends here
