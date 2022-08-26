;;; init-clojure.el --- Clojure Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;(require 'init-lisp)

(use-package yasnippet
  :diminish yas-minor-mode)

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  (subword-mode +1)

  ;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2))

  ;; Always show more of the path in clj buffer names.
  ;; Using setq-local in clojure-mode-hook is not enough, as it runs too late
  (defun clj-uniquify-get-proposed-name (orig base dirname &optional depth original-dirname)
    (when (and (string= ".clj" (substring base -4))
               (not (string= "project.clj" base)))
      (setq-local uniquify-min-dir-content 3))
    (funcall orig base dirname depth original-dirname))
  (advice-add 'uniquify-get-proposed-name :around 'clj-uniquify-get-proposed-name))

(use-package hydra)
(use-package clj-refactor
  :diminish
  :after yasnippet
  :bind ("C-c '" . hydra-cljr-help-menu/body)
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-add-ns-to-blank-clj-files nil) ; disable clj-refactor adding ns to blank files
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (defun clj-refactor-hook-fn ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1))
  :hook
  (clojure-mode . clj-refactor-hook-fn))

(use-package cider
  :diminish
  :config
  (defun cider-repl-mode-hook-fn ()
    (display-line-numbers-mode -1)
    (subword-mode +1))
  (setq cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-display-help-banner nil
        cider-repl-history-highlight-current-entry t
        cider-repl-history-highlight-inserted-item t
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-save-file-on-load t
        cider-test-show-report-on-success t
        ;; cider-invert-insert-eval-p t
        ;; cider-switch-to-repl-on-insert nil
        cider-xref-fn-depth 90
        cider-repl-history-file ".cider-repl-history"
        nrepl-log-messages t
        cider-connection-message-fn nil
        clojure-toplevel-inside-comment-form t)
  (unbind-key "C-c C-l" cider-mode-map)
  :bind
  (:map cider-mode-map ("C-c M-l" . cider-load-file))
  (:map clojure-mode-map
        ("C-x p q" . project-clojure-test-switch)
        ("C->" . cider-find-dwim-other-window))
  :hook
  (cider-repl-mode . cider-repl-mode-hook-fn)
  (cider-mode . eldoc-mode))

(provide 'init-clojure)
;;; init-clojure.el ends here
