;;; init-emacs-lisp.el --- Emacs Lisp Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Based on prelude-emacs-lisp.el
;;; Code:

(require 'init-lisp)

(use-feature eldoc
  :diminish)

(use-package elisp-slime-nav
  :diminish)

(use-package emacs
  :config
  (defun eval-region-or-defun (edebug-it)
    "Call eval-region, if one is selected, or eval-defun otherwise."
    (interactive "P")
    (if (use-region-p)
        (eval-region (region-beginning) (region-end))
      (eval-defun edebug-it)))
  ;; Based on prelude-emacs-lisp.el
  (defun recompile-init-lisp ()
    (when (and
           (string-prefix-p (expand-file-name "lisp" user-emacs-directory) (file-truename buffer-file-name))
           (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (emacs-lisp-byte-compile)))
  (defun recompile-init-lisp-on-save ()
    "Recompile your elc when saving an elisp file. (Adds buffer-local hook)"
    (add-hook 'after-save-hook 'recompile-init-lisp nil t))
  ;; From prelude-emacs-lisp.el
  (defun visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))
  :hook
  (ielm-mode . (lambda ()
                 (eldoc-mode +1)
                 (rainbow-delimiters-mode +1)))
  (emacs-lisp-mode . (lambda ()
                       (eldoc-mode +1)
                       (rainbow-mode +1)
                       (rainbow-delimiters-mode +1)
                       (setq mode-name "EL")
                       (recompile-init-lisp-on-save)))
  :bind
  (:map emacs-lisp-mode-map
        (("C-c C-z" . visit-ielm)
         ("C-M-x" . eval-region-or-defun)
         ("C-c C-c" . eval-region-or-defun)
         ("C-c C-b" . eval-buffer)
         ("C-c e f" . emacs-lisp-byte-compile-and-load)
         ("C-c e z" .  byte-recompile-directory)
         ("C-c e c" . cancel-debug-on-entry)
         ("C-c e d" . debug-on-entry)
         ("C-c e e" . toggle-debug-on-error))))

(provide 'init-emacs-lisp)
;;; init-emacs-lisp.el ends here
