;;; init-tramp.el --- Tramp Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tramp
  ;; TODO remove after next release
  :ensure (:ref "4ae9b86ab682b699994bff58b7560ed1c9c47ece" :pin t)
  :defer 8
  :config
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
  :custom
  (tramp-default-method "ssh")
  (tramp-terminal-type "tramp")
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp))
  (tramp-auto-save-directory (expand-file-name "tramp-auto-save" user-emacs-directory))
  (tramp-persistency-file-name (expand-file-name "tramp-connection-history" user-emacs-directory))
  (password-cache-expiry nil))

(use-package tramp-hlo
  :defer 11
  :config
  (tramp-hlo-setup))

(provide 'init-tramp)
;;; init-tramp.el ends here
