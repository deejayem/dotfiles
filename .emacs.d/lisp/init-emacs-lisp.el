;;; init-emacs-lisp.el --- Emacs Lisp Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Based on prelude-emacs-lisp.el
;;; Code:

(require 'init-lisp)

(use-package eldoc
  :ensure nil
  :diminish)

(use-package elisp-slime-nav
  :diminish)

(use-package emacs
  :ensure nil
  :config
  ;; From prelude-emacs-lisp.el
  (defun recompile-elc-on-save ()
    "Recompile your elc when saving an elisp file. (Adds buffer-local hook)"
    (add-hook 'after-save-hook
              (lambda ()
                (when (and
                       (string-prefix-p user-emacs-directory (file-truename buffer-file-name))
                       (file-exists-p (byte-compile-dest-file buffer-file-name)))
                  (emacs-lisp-byte-compile)))
              nil
              t))
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
                       (recompile-elc-on-save)))
  :bind
  (:map emacs-lisp-mode-map
        (("C-c C-z" . visit-ielm)
         ("C-c C-c" . eval-defun)
         ("C-c C-b" . eval-buffer))))

(provide 'init-emacs-lisp)
;;; init-emacs-lisp.el ends here
