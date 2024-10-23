;;; init-dired.el --- Dired Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-feature dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :custom
  (dired-use-ls-dired t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (wdired-use-dired-vertical-movement 'sometimes)
  (dired-vc-rename-file t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  :bind (:map dired-mode-map
              ("M-o" . dired-omit-mode)
              ("E" . wdired-change-to-wdired-mode)))

(use-feature dired-x)

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-feature casual-dired
  :after dired
  :bind (:map dired-mode-map
              ("C-o" . casual-dired-tmenu)
              ("s" . casual-dired-sort-by-tmenu)))

(provide 'init-dired)
;;; init-dired.el ends here
