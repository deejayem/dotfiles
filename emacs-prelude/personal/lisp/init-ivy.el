(prelude-require-package 'ivy-prescient)
(use-package ivy-prescient
  :diminish
  :config
  (ivy-prescient-mode 1))

(prelude-require-package 'ivy-rich)
(use-package ivy-rich
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :config
  (setq ;counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        counsel-rg-base-command "rg -S -M 120 --no-heading --line-number --color never %s .")
  :bind (("C-c h" . counsel-command-history)
         ;("C-c i" . counsel-git)
         ;("C-c j" . counsel-git-grep)
	 ("C-c c g" . counsel-grep)
         ("C-c c r" . counsel-rg)
         ("C-c c o" . counsel-outline)
         ("C-c z" . counsel-fzf)
         ("C-c c z" . counsel-fzf)
	 ("C-c c a" . counsel-apropos)
         ("C-c c f" . counsel-recentf)
         ("C-c C-f" . counsel-recentf)
         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)
         ;([remap recentf-open-files] . counsel-recentf)
         ([remap dired] . counsel-dired))
    :hook ((after-init . ivy-mode)
          (ivy-mode . counsel-mode)))

(defun counsel-projectile-rg-glob ()
  (interactive)
  (let ((glob (ivy-completing-read "Glob?: " '("*.cljs"
                                               "*.clj"
                                               "*.md"
                                               "*.css"))))
    (counsel-projectile-rg (concat "--glob " glob))))

(prelude-require-package 'counsel-projectile)
(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  :bind (:map projectile-mode-map
         ("C-c p s R" . counsel-projectile-rg-glob)))
         
(provide 'init-ivy)

