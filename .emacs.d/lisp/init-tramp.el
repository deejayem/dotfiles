;;; init-tramp.el --- Tramp Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-feature tramp
  :defer 5
  :config
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
  :custom
  (tramp-default-method "ssh")
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp))
  (tramp-auto-save-directory (expand-file-name "tramp-auto-save" user-emacs-directory))
  (tramp-persistency-file-name (expand-file-name "tramp-connection-history" user-emacs-directory))
  (password-cache-expiry nil))

(provide 'init-tramp)
;;; init-tramp.el ends here
