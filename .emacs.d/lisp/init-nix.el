;;; init-nix.el --- Nix Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nix-mode
  :commands nix-repl-show)

(use-package nix-update
  :commands nix-update-fetch)

(provide 'init-nix)
;;; init-nix.el ends here
