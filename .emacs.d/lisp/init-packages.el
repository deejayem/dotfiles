;;; init-packages.el --- Package Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Elpaca installer block
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
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
;; End of elpaca installer block

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t
        use-package-always-defer t
        package-native-compile t
        elpaca-queue-limit 10)
  (setq use-package-verbose init-file-debug
        use-package-expand-minimally (not init-file-debug)
        use-package-compute-statistics nil
        debug-on-error init-file-debug)
  (bind-key "C-c e u" 'elpaca-fetch-all)
  (bind-key "C-c e m" 'elpaca-manager))

(elpaca diminish)

;; Temporary workaround for packages needing newer version of seq (https://github.com/progfolio/elpaca/issues/216#issuecomment-1868444883))
(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(elpaca `(seq :build ,(+elpaca-seq-build-steps)))

;; Block until current queue processed.
(elpaca-wait)

(add-to-list 'elpaca-ignored-dependencies 'transient)
(add-to-list 'elpaca-ignored-dependencies 'project)

;; https://github.com/progfolio/elpaca/wiki/Logging#auto-hiding-the-elpaca-log-buffer
(defvar +elpaca-hide-log-commands '(eval-buffer eval-region eval-defun eval-last-sexp org-ctrl-c-ctrl-c eros-eval-defun eros-eval-last-sexp elisp-eval-region-or-buffer)
  "List of commands for which a successfully processed log is auto hidden.")
(defun +elpaca-hide-successful-log ()
  "Hide Elpaca log buffer if queues processed successfully."
  (message "this: %S last: %S" this-command last-command)
  (if-let ((incomplete (cl-find 'incomplete elpaca--queues :key #'elpaca-q<-status))
           ((elpaca-q<-elpacas incomplete)))
      nil
    (when-let ((log (bound-and-true-p elpaca-log-buffer))
               (window (get-buffer-window log t)) ;; log buffer visible
               ((or (member last-command +elpaca-hide-log-commands)
                    (member this-command +elpaca-hide-log-commands))))
      (with-selected-window window (quit-window 'kill window)))))
(add-hook 'elpaca-post-queue-hook #'+elpaca-hide-successful-log)

;; https://github.com/progfolio/elpaca/wiki/Logging#how-to-change-a-commands-log-query
(with-eval-after-load 'elpaca-log
  (setf (alist-get +elpaca-hide-log-commands
                   elpaca-log-command-queries nil nil #'equal)
        "#unique | !finished"))

;; https://github.com/progfolio/elpaca/wiki/Logging#customizing-the-position-of-the-elpaca-log-buffer
(add-to-list 'display-buffer-alist '("\\*elpaca-log\\*" (display-buffer-reuse-window display-buffer-at-bottom)))

;; https://github.com/radian-software/radian/blob/e3aad124c8e0cc870ed09da8b3a4905d01e49769/emacs/radian.el#L352
(defmacro use-feature (name &rest args)
  "Like `use-package', but without elpaca integration.
`NAME' and `ARGS' are as with `use-package'"
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

;; useful for corfu and vertico extensions
(defmacro use-extension (pkg name &rest args)
  "Like `use-package', but for a package extension.
`PKG' is the name of the package, `NAME' and `ARGS' are as with `use-package'"
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
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

;; Built-in in 29.1+, but we want the latest
(use-package transient)

(provide 'init-packages)
;;; init-packages.el ends here
