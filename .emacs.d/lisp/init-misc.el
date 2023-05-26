;;; init-misc.el --- Miscellaneous Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :defer 5
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package envrc
  :diminish
  :hook (elpaca-after-init . envrc-global-mode))

(use-package restclient
  :config
  (defvar restclient-saved-requests nil)
  (defun restclient-save-current (label)
    "Save the current request as `label' (or use a default based on method, url and entity)."
    (interactive "sLabel: ")
    (restclient-http-parse-current-and-do
     `(lambda (method url headers entity)
        (let ((lab (if (string-empty-p ,label)
                       (format "%s %s (%s ...)" method url (substring entity 0 (min (length entity) 200)))
                     ,label)))
          (push (cons lab (list method url headers entity nil nil)) restclient-saved-requests)))))

  (defun restclient-delete-saved-request ()
    "Delete a saved request."
    (interactive)
    (if (= 0 (length restclient-saved-requests))
        (message "No saved restclient requests to delete.")
      (setq restclient-saved-requests
            (assoc-delete-all (completing-read "Delete: " (map-keys restclient-saved-requests)) restclient-saved-requests))))

  (defun restclient-call-saved-request ()
    "Call a saved request."
    (interactive)
    (if (= 0 (length restclient-saved-requests))
        (message "No saved restclient requests found.")
      (let ((args (if (= 1 (length restclient-saved-requests))
                      (cdar restclient-saved-requests)
                    (alist-get (completing-read "Call: " (map-keys restclient-saved-requests)) restclient-saved-requests nil nil 'string-equal))))
        (apply 'restclient-http-do args))))
  :bind
  ("C-c C-h" . restclient-call-saved-request)
  (:map restclient-mode-map ("C-c h" . restclient-save-current))
  :mode (("\\.http\\'" . restclient-mode)))

(use-package restclient-jq
  :after restclient
  :demand t)

(use-package es-mode
  :mode "\.es\'")

(use-package json-mode)
(use-package jq-format)
(use-package csv-mode)

(use-package yaml-mode
  :diminish
  :hook
  (yaml-mode . whitespace-mode)
  (yaml-mode . subword-mode))

(provide 'init-misc)
;;; init-misc.el ends here
