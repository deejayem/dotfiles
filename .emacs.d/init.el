;;; init.el --- Init File -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(setq load-prefer-newer t
      native-comp-async-report-warnings-errors nil
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(defvar save-dir (expand-file-name "save" user-emacs-directory))
(unless (file-exists-p save-dir)
  (make-directory save-dir))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defgroup djm nil
  "Custom variables added by me."
  :group 'convenience)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "contrib" user-emacs-directory))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "%s packages loaded in %0.1f seconds"
                     (hash-table-count straight--profile-cache)
                     (string-to-number (emacs-init-time)))))

(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold (* 100 1024 1024)
                                     gc-cons-percentage 0.1)))

;; Some straight functions need to be able to reload everything, so require won't do
(defun require! (feature &optional filename noerror)
  "Like `require', but if `force-reload' is non-nil, `load' instead.
`FEATURE', `FILENAME' and `NOERROR' have the same meaning as with require"
  (if (and (boundp 'force-reload) force-reload)
      (load (prin1-to-string feature) noerror nil nil t)
    (require feature filename noerror)))

(require! 'init-packages)
(require! 'init-ui)
(require! 'init-compile)
(require! 'init-editor)
(require! 'init-search)
(require! 'init-windows)
(require! 'init-project)
(require! 'init-modeline)
(require! 'init-completion)
(require! 'init-minibuffer)
(require! 'init-navigation)
(require! 'init-kill)
(require! 'init-dired)
(require! 'init-smartparens)
(require! 'init-emacs-lisp)
(require! 'init-clojure)
(require! 'init-crux)
(require! 'init-lsp)
(require! 'init-git)
(require! 'init-shell)
(require! 'init-nix)
(require! 'init-org)
;;(require! 'init-latex)
(require! 'init-xml)
(require! 'init-web)
(require! 'init-misc)
(require! 'init-tramp)
(require! 'init-sql)
(require! 'init-local nil t)

;;; init.el ends here
