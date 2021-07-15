(require 'init-lisp)

(prelude-require-package 'yasnippet)
(use-package yasnippet
  :diminish yas-minor-mode)

(prelude-require-package 'flycheck-clj-kondo)
(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))

(prelude-require-package 'clj-refactor)
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
  :after key-chord
  :config
  (setq cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-display-help-banner nil
        cider-repl-history-highlight-current-entry t
        cider-repl-history-highlight-inserted-item t
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-invert-insert-eval-p t
        cider-switch-to-repl-on-insert nil
        clojure-toplevel-inside-comment-form t)
  (key-chord-define-global "??" 'cider-xref-fn-refs-select)
  (key-chord-define-global "qq" 'cider-xref-fn-refs))

(provide 'init-clojure)
