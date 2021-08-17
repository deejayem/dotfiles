;;; init-dired.el --- Dired Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (when (eq system-type 'darwin)
    (setq insert-directory-program "/usr/local/bin/gls"))
  :custom
  (dired-use-ls-dired t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (wdired-use-dired-vertical-movement 'sometimes))

(use-package dired-x
  :ensure nil)

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(provide 'init-dired)
;;; init-dired.el ends here
