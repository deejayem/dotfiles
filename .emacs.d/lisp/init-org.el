;;; init-org.el --- Org-Mode Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Some parts copied from prelude-org.el
;;; Code:

(use-package org
  :custom
  (org-log-done t)
  (org-special-ctrl-k t)
  (org-special-ctrl-a t)
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (elasticsearch . t)
     (clojure . t)
     (restclient . t)
     (sql . t)))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c b" . org-switchb)
  ;; TODO bindings
  ;("C-c r" . org-refile)
  ;("C-c c" . org-capture)
  )

(use-package ob-restclient
  :after org)

(use-package ob-async
  :after org)

(provide 'init-org)
;;; init-org.el ends here
