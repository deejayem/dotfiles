(use-package magit
  :after key-chord
  :init
  (defun my/magit-set-upstream ()
    (interactive)
    (magit-shell-command-topdir "git upstream"))
  :config
  (key-chord-define-global "UU" 'my/magit-set-upstream)
  ;; prevent the git info on the modeline from becoming stale (based on code removed from doom modeline)
  :hook (magit-post-refresh .
    (lambda ()
      (dolist (buf (buffer-list))
        (when (and (not (buffer-modified-p buf))
                   (buffer-file-name buf)
                   (file-exists-p (buffer-file-name buf))
                   (file-in-directory-p (buffer-file-name buf) (magit-toplevel)))
          (with-current-buffer buf
            (vc-refresh-state)))))))

(prelude-require-package 'forge)
(use-package forge
  :after magit)

(provide 'init-git)

