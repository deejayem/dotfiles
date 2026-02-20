;;; init-packages.el --- Package Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Elpaca installer block
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
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
  (bind-key "C-c e m" 'elpaca-manager)
  (bind-key "C-c e r" 'elpaca-update-menus)
  (bind-key "C-c e t" 'elpaca-try)
  (bind-key "C-c e b" 'elpaca-rebuild)
  (bind-key "C-c e d" 'elpaca-delete))

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

(defun +elpaca-unload-transient (e)
  (and (featurep 'transient) (unload-feature 'transient t))
  (elpaca--continue-build e))

(defun +elpaca-transient-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "transient" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-transient 'elpaca--activate-package)))

(elpaca `(transient :build ,(+elpaca-transient-build-steps)))

;; Block until current queue processed.
(elpaca-wait)

(add-to-list 'elpaca-ignored-dependencies 'project)
(add-to-list 'elpaca-ignored-dependencies 'xref)

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

;; https://github.com/progfolio/elpaca/wiki/Reloading-a-package%E2%80%99s-features-after-updating-a-package
(defun +elpaca-reload-package (package &optional allp)
  "Reload PACKAGE's features.
If ALLP is non-nil (interactively, with prefix), load all of its
features; otherwise only load ones that were already loaded.

This is useful to reload a package after upgrading it.  Since a
package may provide multiple features, to reload it properly
would require either restarting Emacs or manually unloading and
reloading each loaded feature.  This automates that process.

Note that this unloads all of the package's symbols before
reloading.  Any data stored in those symbols will be lost, so if
the package would normally save that data, e.g. when a mode is
deactivated or when Emacs exits, the user should do so before
using this command."
  (interactive
   (list (let ((elpaca-overriding-prompt "Reload package: "))
           (elpaca--read-queued))
         current-prefix-arg))
  ;; This finds features in the currently installed version of PACKAGE, so if
  ;; it provided other features in an older version, those are not unloaded.
  (when (yes-or-no-p (format "Unload all of %s's symbols and reload its features? " package))
    (let* ((package-name (symbol-name package))
           (package-dir (file-name-directory
                         (locate-file package-name load-path (get-load-suffixes))))
           (package-files (directory-files package-dir 'full (rx ".el" eos)))
           (package-features
            (cl-loop for file in package-files
                     when (with-temp-buffer
                            (insert-file-contents file)
                            (when (re-search-forward (rx bol "(provide" (1+ space)) nil t)
                              (goto-char (match-beginning 0))
                              (cadadr (read (current-buffer)))))
                     collect it)))
      (unless allp
        (setf package-features (seq-intersection package-features features)))
      (dolist (feature package-features)
        (ignore-errors
          ;; Ignore error in case it's not loaded.
          (unload-feature feature 'force)))
      (dolist (feature package-features)
        (require feature))
      (when package-features
        (message "Reloaded: %s" (mapconcat #'symbol-name package-features " "))))))

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

(use-feature elpaca-ui
  :config
  (defun +elpaca-ui-mark-merge-next-package ()
    "Mark current package for merge and move to the next package."
    (interactive)
    (let ((pkg (elpaca-ui-current-package)))
      (elpaca-ui-mark-merge)
      (while (and (not (eobp))
                  (eq pkg (elpaca-ui-current-package)))
        (forward-line 1))))
  (defun +elpaca-backup-lock-file (&rest _)
    "Backup elpaca lock file before executing marked packages."
    (when-let* ((lock-file (expand-file-name "elpaca.lock" user-emacs-directory))
                ((file-exists-p lock-file)))
      (let* ((state-dir (or (getenv "XDG_STATE_HOME")
                            (expand-file-name ".local/state/" "~")))
             (backup-dir (expand-file-name "lock-backups/elpaca/" state-dir))
             (timestamp (format-time-string "%Y-%m-%dT%H-%M-%S"))
             (backup-file (expand-file-name
                           (concat "elpaca.lock." timestamp) backup-dir)))
        (make-directory backup-dir t)
        (copy-file lock-file backup-file t)
        (message "Backed up elpaca.lock to %s" backup-file)
        (dolist (old-file (directory-files backup-dir t "\\`elpaca\\.lock\\."))
          (when (> (float-time (time-since (file-attribute-modification-time
                                            (file-attributes old-file))))
                   (* 30 24 60 60))
            (delete-file old-file))))))
  (advice-add 'elpaca-ui-execute-marks :before #'+elpaca-backup-lock-file)
  (defun +elpaca-ui-execute-marks-and-write-lockfile ()
    "Execute marks and write lockfile."
    (interactive)
    (elpaca-ui-execute-marks)
    (add-to-list 'elpaca--post-queues-hook
                 (lambda () (elpaca-write-lock-file
                             (expand-file-name "elpaca.lock" user-emacs-directory)))
                 t))
  :bind (:map elpaca-ui-mode-map
              ("M" . +elpaca-ui-mark-merge-next-package)
              ("X" . +elpaca-ui-execute-marks-and-write-lockfile)))

(provide 'init-packages)
;;; init-packages.el ends here
