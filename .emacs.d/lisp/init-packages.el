;;; init-packages.el --- Package Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t
        use-package-always-defer t
        package-native-compile t
        elpaca-queue-limit 10)
  (bind-key "C-c e u" 'elpaca-fetch-all)
  (bind-key "C-c e m" 'elpaca-manager))

(elpaca diminish)

;; Block until current queue processed.
(elpaca-wait)

;; https://github.com/radian-software/radian/blob/e3aad124c8e0cc870ed09da8b3a4905d01e49769/emacs/radian.el#L352
(defmacro use-feature (name &rest args)
  "Like `use-package', but with `elpaca-use-package-by-default' disabled.
`NAME' and `ARGS' are as with `use-package'"
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     ,@args))

;; useful for corfu and vertico extensions
(defmacro use-extension (pkg name &rest args)
  "Like `use-package', but for a package extension.
`PKG' is the name of the package, `NAME' and `ARGS' are as with `use-package'"
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     :after ,pkg
     :demand t
     ,@args))

(defun add-to-list* (list-var &rest elts)
  "Add `ELTS' to `LIST-VAR'."
  (dolist (elt elts)
    (add-to-list list-var elt)))

(defun append-to-list* (list-var &rest elts)
  "Append `ELTS' to `LIST-VAR'."
  (dolist (elt elts)
    (add-to-list list-var elt t)))

(provide 'init-packages)
;;; init-packages.el ends here
