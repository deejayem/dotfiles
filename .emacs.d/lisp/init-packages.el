;;; init-packages.el --- Package Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(find-when-checking check-on-save)
      use-package-always-defer t
      package-native-compile t)

(defvar bootstrap-version)
 (let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 6))
   (unless (file-exists-p bootstrap-file)
     (with-current-buffer
         (url-retrieve-synchronously
          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
       (goto-char (point-max))
       (eval-print-last-sexp)))
   (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; https://github.com/radian-software/radian/blob/e3aad124c8e0cc870ed09da8b3a4905d01e49769/emacs/radian.el#L352
(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
`NAME' and `ARGS' are as with `use-package'"
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))
(use-feature straight-x
  :commands (straight-x-fetch-all))

;; useful for corfu and vertico extenions
(defmacro use-extension (pkg name &rest args)
  "Like `use-package', but for a package extension.
`PKG' is the name of the package, `NAME' and `ARGS' are as with `use-package'"
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     :after pkg
     :demand t
     ,@args))

(use-package diminish)

(defun run-straight-lock-file-function (func)
  "Safely run straight lockfile-related function `FUNC'.
This will remove all init-* files from `features', so that they are reloaded."
  (setq features (seq-filter '(lambda (elt) (not (string-prefix-p "init-" (prin1-to-string elt)))) features))
  (funcall func))

(defun reload-init ()
  "Reload `user-init-file', ensuring that requires are reloaded."
  (run-straight-lock-file-function #'(lambda () (load (or user-init-file "~/.emacs.d/init.el") nil 'nomessage))))

;; emacs --batch -l "~/.emacs.d/init.el" -f "my/upgrade-packages"
(defun my/upgrade-packages ()
  "Upgrade all packages installed with straight."
  (interactive)
  (straight-pull-recipe-repositories) ;; TODO is this needed?
  (straight-x-fetch-all)
  (straight-merge-all)
  (reload-init)
  (straight-check-all)
  ;; Do this automatically, as we can always revert and thaw
  (run-straight-lock-file-function 'straight-freeze-versions))

;; emacs --batch -l "~/.emacs.d/init.el" -f "my/thaw-packages"
(defun my/thaw-packages ()
  "Restore all packages to the versions in the straight lockfile."
  (interactive)
  (run-straight-lock-file-function 'straight-thaw-versions))

(provide 'init-packages)
;;; init-packages.el ends here
