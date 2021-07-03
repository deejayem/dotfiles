(prelude-require-package 'simple-modeline)

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode))

(prelude-require-package 'flycheck-indicator)
(use-package flycheck-indicator
  :after flycheck
  :hook (flycheck-mode . flycheck-indicator-mode)
  :custom
  (flycheck-indicator-icon-error 9632)
  (flycheck-indicator-icon-info 9679)
  (flycheck-indicator-icon-warning 9650)
  (flycheck-indicator-status-icons
   '((running . "◉")
     (errored . "◙")
     (finished . "●")
     (interrupted . "◘")
     (suspicious . "◘")
     (no-checker . "○")
     (not-checked . "○"))))

(provide 'init-modeline)

