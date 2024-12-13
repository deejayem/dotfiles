;;; init.el --- Init File -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(defvar save-dir (expand-file-name "save" user-emacs-directory))
(unless (file-exists-p save-dir)
  (make-directory save-dir))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defgroup djm nil
  "Custom variables added by me."
  :group 'convenience)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "contrib" user-emacs-directory))

(defun display-startup-echo-area-message ()
  "Custom version of `display-startup-echo-area-message'."
  (message "%s packages loaded in %0.1f seconds"
           (cdar elpaca--status-counts)
           (string-to-number (emacs-init-time))))

(add-hook 'elpaca-after-init-hook #'(lambda ()
                                      (setq gc-cons-threshold (* 100 1024 1024)
                                            gc-cons-percentage 0.1
                                            ;; https://github.com/magnars/emacsd-reboot/blob/44ebe6b5f80deebe0907be55f206a0a3f7fc9fcd/settings/fast-startup.el#L28-29 
                                            (setq file-name-handler-alist file-name-handler-alist-original)
                                            (makunbound 'file-name-handler-alist-original)))
          99)

(require 'init-packages)
(require 'init-ui)
(require 'init-compile)
(require 'init-editor)
(require 'init-search)
(require 'init-windows)
(require 'init-project)
(require 'init-modeline)
(require 'init-completion)
(require 'init-minibuffer)
(require 'init-navigation)
(require 'init-kill)
(require 'init-dired)
(require 'init-smartparens)
(require 'init-emacs-lisp)
(require 'init-clojure)
(require 'init-crux)
(require 'init-lsp)
(require 'init-git)
(require 'init-shell)
(require 'init-nix)
(require 'init-org)
;;(require 'init-latex)
(require 'init-xml)
(require 'init-web)
(require 'init-misc)
(require 'init-tramp)
(require 'init-sql)
(require 'init-local nil t)

;;; init.el ends here
