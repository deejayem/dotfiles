;; config for misc packages included with prelude

(use-package flycheck
  :diminish
  :config
  (global-flycheck-mode))

(use-package which-key
  :diminish)

(use-package whitespace
  :diminish)

(use-package ace-window
  :diminish
  :custom-face
  (aw-leading-char-face
    ((t (:foreground "white" :background "red"
         :weight bold :height 2.5 :box (:line-width 10 :color "red"))))))

(provide 'init-prelude)

