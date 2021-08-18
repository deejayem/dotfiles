;;; init-projectile.el --- Projectile Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom projectile-default-dir "~/src"
  "Starting directory when looking for new projects."
  :group 'djm
  :type 'directory)

(defcustom projectile-switch-project-command 'projectile-persp-switch-project
  "Projectile switch project command."
  :group 'djm
  :type 'function)

(use-package projectile
  :diminish
  :after smartrep
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (def-projectile-commander-method ?b
    "consult-buffer"
    (progn
      (setq unread-command-events (listify-key-sequence "p "))
      (consult-buffer)))
  (def-projectile-commander-method ?B
    "Switch to project buffer"
    (projectile-switch-to-buffer))
  (def-projectile-commander-method ?r
    "consult-ripgrep"
    (consult-ripgrep))
  (def-projectile-commander-method ?p
    "DWIM"
    (cond ((> (length (projectile-project-buffer-names)) 4) (projectile-switch-to-buffer))
          ((> (length (projectile-recentf-files)) 0) (projectile-recentf))
          (t (projectile-find-file))))
  (defun projectile-open-new-project (project-root)
    (interactive (list (read-directory-name "Select project directory: " (file-name-as-directory  projectile-default-dir))))
    (projectile-add-known-project project-root)
    (funcall projectile-switch-project-command project-root))
  (smartrep-define-key projectile-mode-map
      "C-c p" '(("C-p" . projectile-previous-project-buffer)
                ("C-n" . projectile-next-project-buffer)))
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-mode-map ("C-c p n" . projectile-open-new-project))
  :custom
  (projectile-switch-project-action 'projectile-commander)
  (projectile-cache-file (expand-file-name  "projectile.cache" save-dir)))

(use-package perspective
  :init (persp-mode)
  :bind ("C-x C-b" . persp-ibuffer)
  :custom (persp-modestring-short t))

(use-package persp-projectile)

(provide 'init-projectile)
;;; init-projectile.el ends here
