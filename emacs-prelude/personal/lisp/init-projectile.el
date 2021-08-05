(prelude-require-packages '(perspective persp-projectile ripgrep))

(use-package perspective
 :init (persp-mode)
 :custom (persp-modestring-short t))

(use-package persp-projectile)

(use-package projectile
  :diminish
  :config
  (def-projectile-commander-method ?B
    "consult-buffer"
    (progn
      (setq unread-command-events (listify-key-sequence "p "))
      (consult-buffer)))
  (def-projectile-commander-method ?r
    "consult-ripgrep"
    (consult-ripgrep))
  (def-projectile-commander-method ?p
    "DWIM"
    (cond ((> (length (projectile-project-buffer-names)) 4) (projectile-switch-to-buffer)) ;; TODO consult-buffer
          ((> (length (projectile-recentf-files)) 0) (projectile-recentf))
          (t (projectile-find-file))))
  :custom
  (projectile-switch-project-action 'projectile-commander))


(provide 'init-projectile)
