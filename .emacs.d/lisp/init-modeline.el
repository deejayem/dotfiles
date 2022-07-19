;;; init-modeline.el --- Modeline Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(line-number-mode t)
(column-number-mode t)
;(size-indication-mode t) ; TODO

(use-package which-func
  :ensure nil
  :config
  (which-function-mode 1))

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode)
  :custom
  (simple-modeline-segments
   '((simple-modeline-segment-modified
      simple-modeline-segment-buffer-name
      simple-modeline-segment-position)
     (simple-modeline-segment-minor-modes
      simple-modeline-segment-vc
      simple-modeline-segment-misc-info
      simple-modeline-segment-process
      simple-modeline-segment-major-mode))))

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
;;; init-modeline.el ends here
