;;; init-compile.el --- Compilation Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Based on code from prelude-editor.el
;;; Code:

(use-package compile
  :custom
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error))

;; http://stackoverflow.com/a/3072831/355252
(use-package ansi-color
  :hook
  (compilation-filter . (lambda ()
                          (when (eq major-mode 'compilation-mode)
                            (let ((inhibit-read-only t))
                              (ansi-color-apply-on-region (point-min) (point-max)))))))

(use-package winnow
  :hook (compilation-mode . winnow-mode))

(provide 'init-compile)
;;; init-compile.el ends here
