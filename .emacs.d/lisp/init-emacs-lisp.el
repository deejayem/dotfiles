;;; init-emacs-lisp.el --- Emacs Lisp Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Based on prelude-emacs-lisp.el
;;; Code:

(use-feature eldoc
  :diminish)

(use-package elisp-slime-nav
  :diminish)

(use-feature emacs
  :custom
  ;; These don't really belong here, but do affect ielm
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048)
  :config
  ;; Based on prelude-emacs-lisp.el
  (defun recompile-init-lisp ()
    "Recompile elisp files in `user-emacs-directory/lisp'."
    (interactive)
    (when (and
           (string-prefix-p (expand-file-name "lisp" user-emacs-directory) (file-truename buffer-file-name))
           (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (emacs-lisp-byte-compile)))
  (defun recompile-init-lisp-on-save ()
    "Recompile your elc when saving an elisp file. (Adds buffer-local hook)."
    (add-hook 'after-save-hook 'recompile-init-lisp nil t))
  ;; From prelude-emacs-lisp.el
  (defun visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))
  ;; from https://www.n16f.net/blog/making-ielm-more-comfortable/
  (defun ielm-init-history ()
    (let ((path (expand-file-name "ielm/history" user-emacs-directory)))
      (make-directory (file-name-directory path) t)
      (setq-local comint-input-ring-file-name path))
    (setq-local comint-input-ring-size 10000)
    (setq-local comint-input-ignoredups t)
    (comint-read-input-ring))
  (defun ielm-write-history (&rest _args)
    (with-file-modes #o600
      (comint-write-input-ring)))
  :hook
  (ielm-mode . (lambda ()
                 (eldoc-mode +1)
                 (ielm-init-history)
                 (advice-add 'ielm-send-input :after 'ielm-write-history)))
  (emacs-lisp-mode . (lambda ()
                       (eldoc-mode +1)
                       (setq mode-name "EL")
                       (recompile-init-lisp-on-save)))
  :bind
  (:map emacs-lisp-mode-map
        (("C-c C-z" . visit-ielm)
         ("C-c C-c" . eval-defun)
         ("C-c C-k" . eval-buffer)
         ("C-c e f" . emacs-lisp-byte-compile-and-load)
         ("C-c e z" .  byte-recompile-directory)
         ("C-c e c" . cancel-debug-on-entry)
         ("C-c e d" . debug-on-entry)
         ("C-c e e" . toggle-debug-on-error))))


(use-package eros
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package erefactor
  :hook (emacs-lisp-mode . (lambda () (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map))))

(use-package flycheck-package
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

(use-package elsa)

(use-package flycheck-elsa
  :hook
  (emacs-lisp-mode . flycheck-elsa-setup))

(provide 'init-emacs-lisp)
;;; init-emacs-lisp.el ends here
