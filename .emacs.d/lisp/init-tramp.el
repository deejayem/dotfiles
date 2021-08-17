;;; init-tramp.el --- Tramp Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh")
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp))
  (tramp-default-method "ssh")
  (tramp-auto-save-directory (expand-file-name "tramp-auto-save" user-emacs-directory))
  (tramp-persistency-file-name (expand-file-name "tramp-connection-history" user-emacs-directory))
  (password-cache-expiry nil))

(provide 'init-tramp)
;;; init-tramp.el ends here
