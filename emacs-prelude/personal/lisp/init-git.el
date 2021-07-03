(defun magit-set-upstream ()
  (interactive)
  (magit-shell-command-topdir "git upstream"))

(use-package magit
  :after key-chord
  :config
  (key-chord-define-global "UU" 'magit-set-upstream))
;  :bind (("C-c g y" . magit-upstream))) ;; TODO

(use-package forge
  :after magit)

(provide 'init-git)

