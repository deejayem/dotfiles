;;; init-git.el --- VCS/Git Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ediff
  :custom
  (ediff-setup-windows-plain 'ediff-setup-windows-plain))

(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  :hook
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package gitconfig)
(use-package git-modes)

(use-package git-timemachine
  :bind
  ("C-x v t" . git-timemachine-toggle))

(use-package vc
  :bind
  (("C-x v C-r" . my/vc-refresh-state)
   ("C-x v C-m" . my/update-git-master))
  :custom (vc-follow-symlinks nil)
  :config
  (defun my/vc-refresh-state ()
    (interactive)
    (when-let ((root-dir (vc-root-dir)))
      (dolist (buf (buffer-list))
        (when (and (not (buffer-modified-p buf))
                   (buffer-file-name buf)
                   (file-exists-p (buffer-file-name buf))
                   (file-in-directory-p (buffer-file-name buf) root-dir))
          (with-current-buffer buf
            (vc-refresh-state))))))

  ;; [alias]
  ;;   update-master = !git fetch origin master:master
  ;;   update-main = !git fetch origin main:main
  (defun my/update-git-master ()
    "Update git master or main branch."
    (interactive)
    (if-let ((root (vc-root-dir)))
        (let* ((branches (vc-git-branches))
               (main-p (member "main" branches))
               (master-p (member "master" branches))
               (current-branch (car branches))
               (on-master-p (member current-branch '("master" "main")))
               (command (if main-p "update-main" "update-master"))
               (buffer "*vc-update-master*"))
          (if on-master-p
              (vc-pull)
            ;; based on vc-git--pushpull
            (require 'vc-dispatcher)
            (apply #'vc-do-async-command buffer root vc-git-program command nil)
            (with-current-buffer buffer
              (vc-run-delayed
                (vc-compilation-mode 'git)
                (setq-local compile-command
                            (concat vc-git-program " " command))
                (setq-local compilation-directory root)
                (setq-local compilation-arguments
                            (list compile-command nil
                                  (lambda (_name-of-mode) buffer)
                                  nil))))
            (vc-set-async-update buffer)))
      (message "not a git repository"))))

(use-package magit
  :bind
  ("C-c g g" . magit-dispatch) ;; magit-file-dispatch is C-c M-g
  ("C-c g u" . my/magit-set-upstream)
  ("C-c g r" . my/magit-refresh-state)
  ("C-c g m" . my/magit-update-git-master)
  :config
  ;; Requires the following gitconfig:
  ;; [alias]
  ;;   upstream = !git push -u origin HEAD
  ;; TODO - this is useful after setting push remote, but is there a better way?
  (defun my/magit-set-upstream ()
    (interactive)
    (magit-shell-command-topdir "git upstream"))

  ;; update stale git info on the modeline (based on code removed from doom modeline)
  (defun my/magit-refresh-state ()
    "Update modeline git branch information."
    (interactive)
    (dolist (buf (buffer-list))
      (when (and (not (buffer-modified-p buf))
                 (buffer-file-name buf)
                 (file-exists-p (buffer-file-name buf))
                 (file-in-directory-p (buffer-file-name buf) (magit-toplevel)))
        (with-current-buffer buf
          (vc-refresh-state)))))

  ;; [alias]
  ;;   update-master = !git fetch origin master:master
  ;;   update-main = !git fetch origin main:main
  (defun my/magit-update-master ()
    "Update git master or main branch."
    (interactive)
    (if (magit-toplevel)
        (let* ((branches (vc-git-branches))
               (main-p (member "main" branches))
               (current-branch (car branches))
               (on-master-p (member current-branch '("master" "main")))
               (command (concat "git " (if main-p "update-main" "update-master"))))
          (if on-master-p
              (vc-pull)
            (magit-shell-command-topdir command)))
      (message "Not a git repository")))

  ;; Based on https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
  (transient-define-prefix my/magit-extra-commands ()
    "Extra magit commands."
    ["Extra commands"
     ("u" "Set upstream" my/magit-set-upstream)
     ("r" "Refresh state (update modeline)" my/magit-refresh-state)
     ("m" "Update master/main" my/magit-update-master)])
  (transient-append-suffix 'magit-dispatch "!"
    '("#" "Extra Magit Cmds" my/magit-extra-commands))
  (define-key magit-status-mode-map (kbd "#") #'my/magit-extra-commands)

  :custom
  (magit-diff-refine-hunk 'all)
  (magit-diff-paint-whitespace-lines 'all)
  (magit-diff-refine-ignore-whitespace nil)
  (magit-diff-highlight-trailing t))

(use-package magit-todos)

(use-package forge
  :after magit)

(provide 'init-git)
;;; init-git.el ends here
