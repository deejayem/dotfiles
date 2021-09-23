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
  (subword-mode +1))

(use-package hydra)
(use-package clj-refactor
  :diminish
  :after yasnippet
  :bind ("C-c '" . hydra-cljr-help-menu/body)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-suppress-no-project-warning t)
  (defun clj-refactor-hook-fn ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1))
  :hook
  (clojure-mode . clj-refactor-hook-fn))

(use-package cider
  :diminish
  :config
  (defun set-project-repl-history ()
    (let ((project-name (file-name-nondirectory (directory-file-name (project-root (project-current))))))
      (when (> (length project-name) 0)
        (setq-local cider-repl-history-file (expand-file-name (concat "cider-history-" project-name) user-emacs-directory)))))
  (defun cider-repl-mode-hook-fn ()
    (display-line-numbers-mode -1)
    (subword-mode +1)
    (set-project-repl-history))
  (setq cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-display-help-banner nil
        cider-repl-history-highlight-current-entry t
        cider-repl-history-highlight-inserted-item t
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-save-file-on-load t
        ;; cider-invert-insert-eval-p t
        ;; cider-switch-to-repl-on-insert nil
        ;; Default cider-repl-history file
        cider-repl-history-file (expand-file-name "cider-history" user-emacs-directory)
        nrepl-log-messages t
        clojure-toplevel-inside-comment-form t)
  (unbind-key "C-c C-l" cider-mode-map)
  :bind
  (:map cider-mode-map ("C-c M-l" . cider-load-file))
  (:map clojure-mode-map ("C-x p q" . project-clojure-test-switch))
  :hook
  (cider-repl-mode . cider-repl-mode-hook-fn)
  (cider-mode . eldoc-mode))

(provide 'init-clojure)
;;; init-clojure.el ends here
