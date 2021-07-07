(prelude-require-package 'envrc)
(use-package envrc
 :diminish
 :config
 (envrc-global-mode))

(prelude-require-package 'rg)
(use-package rg
  :config
  (rg-enable-default-bindings))

(prelude-require-package 'restclient)
(use-package restclient)

(prelude-require-package 'es-mode)
(use-package es-mode
  :mode "\.es\'")

(provide 'init-misc)

