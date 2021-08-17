;;; init.el --- Init File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq load-prefer-newer t)

(defvar save-dir (expand-file-name "save" user-emacs-directory))
(unless (file-exists-p save-dir)
  (make-directory save-dir))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defgroup djm nil
 "Custom variables added by me"
 :group 'convenience)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-ui)
(require 'init-compile)
(require 'init-editor)
(require 'init-crux)
(require 'init-navigation)
(require 'init-windows)
(require 'init-projectile)
(require 'init-modeline)
(require 'init-dashboard)
(require 'init-completion)
(require 'init-kill)
(require 'init-dired)
(require 'init-smartparens)
(require 'init-lisp)
(require 'init-emacs-lisp)
(require 'init-clojure)
(require 'init-paredit)
(require 'init-paredit-x)
(require 'init-lsp)
(require 'init-git)
(require 'init-org)
;;(require 'init-latex)
(require 'init-xml)
(require 'init-web)
(require 'init-misc)
(require 'init-tramp)
(require 'init-local nil t)

(add-hook 'after-init-hook '(lambda ()
                              (setq gc-cons-threshold (* 100 1024 1024)
                                    gc-cons-percentage 0.1)))
;;; init.el ends here
