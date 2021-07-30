(prelude-require-package 'envrc)
(use-package envrc
 :diminish
 :config
 (envrc-global-mode))

(prelude-require-package 'rg)
(use-package rg
  :config
  (rg-enable-default-bindings))

(prelude-require-package 'restclient)
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

(prelude-require-package 'es-mode)
(use-package es-mode
  :mode "\.es\'")

(prelude-require-package 'multi-vterm)
(use-package multi-vterm
  :init (unbind-key "C-c t" prelude-mode-map)
  :bind (("C-c t" . multi-vterm-next)
         ("C-c C-M-t" . multi-vterm)
         (:map vterm-mode-map
               ("C-a" . vterm-send-C-a) ; TODO the crux binding is taking precedence
               ("C-c C-a" . vterm-send-C-a)
               ("M-[" . multi-vterm-prev)
               ("M-]" . multi-vterm-next))))

(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t))

(prelude-require-package 'eshell-z)
(use-package eshell-z
  :defer t
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

(prelude-require-package 'tagedit)
(use-package tagedit
  :config (tagedit-add-paredit-like-keybindings)
  :hook (html-mode . (lambda () (tagedit-mode 1))))

(provide 'init-misc)
