;;; init-misc.el --- Miscellaneous Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package envrc
 :diminish
 :config
 (envrc-global-mode))

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

(use-package es-mode
  :mode "\.es\'")

(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(use-package json-mode)
(use-package csv-mode)
(use-package yaml-mode
  :diminish
  :hook
  (yaml-mode . whitespace-mode)
  (yaml-mode . subword-mode))

(use-package ovpn-mode)

(provide 'init-misc)
;;; init-misc.el ends here
