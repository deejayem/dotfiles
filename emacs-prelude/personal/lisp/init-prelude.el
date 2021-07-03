;; config for misc packages included with prelude

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package which-key
  :diminish)

(use-package whitespace
  :diminish)

(use-package projectile
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

;; allow other things to use C-c s
(define-key prelude-mode-map (kbd "C-c s") nil)
(define-key prelude-mode-map (kbd "C-c S") 'crux-swap-windows)

(diminish 'prelude-mode)

(provide 'init-prelude)

