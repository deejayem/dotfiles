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

(use-package multi-vterm
  :bind (("C-c t" . multi-vterm-next)
         ("C-c C-M-t" . multi-vterm)
         (:map vterm-mode-map
               ("M-[" . multi-vterm-prev)
               ("M-]" . multi-vterm-next))))

(use-package eshell
  :ensure nil
  :bind ("C-x m " . eshell)
  :custom
  (eshell-directory-name (expand-file-name "eshell" save-dir)))

(use-package eshell-z
  :defer t
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

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
