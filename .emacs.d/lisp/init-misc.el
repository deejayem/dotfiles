;;; init-misc.el --- Miscellaneous Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :defer 5
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package envrc
 :diminish
 :init
 (envrc-global-mode))

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

(use-package restclient-jq
  :after restclient
  :demand t)

(use-package es-mode
  :mode "\.es\'")

(use-package json-mode)
(use-package jq-format)
(use-package csv-mode)

(use-package yaml-mode
  :diminish
  :hook
  (yaml-mode . whitespace-mode)
  (yaml-mode . subword-mode))

(provide 'init-misc)
;;; init-misc.el ends here
