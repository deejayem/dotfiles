;;; init-org.el --- Org-Mode Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Some parts copied from prelude-org.el
;;; Code:

(use-package org
  :ensure nil
  :init
  ;; TODO - can we do this with sp-wrap-with-pair?
  (defmacro define-org-wrap (name char)
    (let ((cmd (intern (concat "org-" name))))
      `(defun ,cmd
           ()
         (interactive)
         (if (use-region-p)
             (let ((re (region-end))
                   (rb (region-beginning)))
               (goto-char re)
               (insert ,char)
               (goto-char rb)
               (insert ,char))
           (beginning-of-thing 'symbol)
           (insert ,char)
           (end-of-thing 'symbol)
           (insert ,char)))
      `(bind-key ,(concat "C-c " (char-to-string char)) ',cmd org-mode-map)))
  :custom
  (org-log-done t)
  (org-special-ctrl-k t)
  (org-special-ctrl-a t)
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (define-org-wrap "underline" ?_)
  (define-org-wrap "bold" ?*)
  (define-org-wrap "italic" ?/)
  (define-org-wrap "verbatim" ?=)
  (define-org-wrap "code" ?~)
  (define-org-wrap "strike-through" ?+)
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
