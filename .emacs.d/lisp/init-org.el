;;; init-ui.el --- Org-Mode Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Some parts copied from prelude-org.el
;;; Code:

(use-package org
  :ensure nil
  :custom (org-log-done t)
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c b" . org-switchb)
  ;; TODO bindings
  ;("C-c r" . org-refile)
  ;("C-c c" . org-capture)
  )

(provide 'init-org)
;;; init-org.el ends here
