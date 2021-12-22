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
(use-package gist)
(use-package git-timemachine)

(use-package magit
  :bind
  ("C-c g" . magit-file-dispatch)
  ("C-c M-g" . magit-dispatch)
  ("C-c C-g u" . my/magit-set-upstream)
  ("C-c C-g r" . my/magit-refresh-state)
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
    (interactive)
    (dolist (buf (buffer-list))
      (when (and (not (buffer-modified-p buf))
                 (buffer-file-name buf)
                 (file-exists-p (buffer-file-name buf))
                 (file-in-directory-p (buffer-file-name buf) (magit-toplevel)))
        (with-current-buffer buf
          (vc-refresh-state)))))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-diff-paint-whitespace-lines 'all)
  (magit-diff-refine-ignore-whitespace nil)
  (magit-diff-highlight-trailing t))

(use-package forge
  :after magit)

(use-package git-gutter
  :diminish
  :bind
  ("C-c j g" . git-gutter-mode)
  ("C-c j S-g" . git-gutter)
  ("C-c j n" . git-gutter:next-hunk)
  ("C-c j p" . git-gutter:previous-hunk)
  ("C-c j r" . git-gutter:revert-hunk)
  ("C-c j d" . git-gutter:popup-hunk))

(provide 'init-git)
;;; init-git.el ends here
