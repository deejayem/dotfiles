(use-package projectile
  :diminish)

(prelude-require-packages '(perspective persp-projectile))
(use-package persp-projectile
  :init (persp-mode))

(provide 'init-projectile)

