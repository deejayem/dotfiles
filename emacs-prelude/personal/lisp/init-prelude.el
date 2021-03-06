;; config for misc packages included with prelude

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package which-key
  :diminish)

(use-package whitespace
  :diminish)

(use-package smartparens
  :diminish)

(use-package subword
  :diminish)

(use-package ace-window
  :diminish
  :custom-face
  (aw-leading-char-face
    ((t (:foreground "white" :background "red"
         :weight bold :height 2.5 :box (:line-width 10 :color "red"))))))

(use-package crux
  ;; allow other things to use C-c s
  :init (unbind-key "C-c s" prelude-mode-map)
  :bind (:map prelude-mode-map ("C-c S" . crux-swap-windows)))

(use-package key-chord
  :init
  (key-chord-unset-global "jl")
  (key-chord-define-global "LL" 'avy-goto-line)
  (key-chord-define-global ",," 'avy-goto-char-in-line))

(diminish 'prelude-mode)

(provide 'init-prelude)

