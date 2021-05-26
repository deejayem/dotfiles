(prelude-require-package 'doom-modeline)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config ;(add-hook 'after-init-hook #'doom-modeline-mode)
  (setq doom-modeline-minor-modes t
        doom-modeline-major-mode-icon t))

(prelude-require-package 'minions)
(use-package minions
  :hook (doom-modeline-mode . minions-mode))


(provide 'init-modeline)

