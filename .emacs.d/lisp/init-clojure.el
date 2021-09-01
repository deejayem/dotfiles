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
  :hook
  (clojure-mode .
    (lambda ()
      (clj-refactor-mode 1)
      (yas-minor-mode 1))))

(use-package cider
  :diminish
  :config
  (setq cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-display-help-banner nil
        cider-repl-history-highlight-current-entry t
        cider-repl-history-highlight-inserted-item t
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-save-file-on-load t
        ;; cider-invert-insert-eval-p t
        ;; cider-switch-to-repl-on-insert nil
        cider-repl-history-file "~/.emacs.d/cider-history"
        nrepl-log-messages t
        clojure-toplevel-inside-comment-form t)
  (unbind-key "C-c C-l" cider-mode-map)
  :bind (:map cider-mode-map ("C-c M-l" . cider-load-file))
  :hook
  (cider-repl-mode . (lambda ()
                       (display-line-numbers-mode -1)
                       (subword-mode +1)))
  (cider-mode . eldoc-mode))

(provide 'init-clojure)
;;; init-clojure.el ends here
