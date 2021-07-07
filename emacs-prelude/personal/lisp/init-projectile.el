(prelude-require-packages '(perspective persp-projectile ripgrep))

(use-package persp-projectile
  :init (persp-mode))

(use-package projectile
  :diminish)

(provide 'init-projectile)

