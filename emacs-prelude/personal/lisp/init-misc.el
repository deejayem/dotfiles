(prelude-require-package 'envrc)
(use-package envrc
 :diminish
 :config
 (envrc-global-mode))

(prelude-require-package 'rg)
(use-package rg
  :config
  (rg-enable-default-bindings))


(provide 'init-misc)

