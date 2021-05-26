(setq tab-always-indent 'complete)

(use-package company
  :bind (("M-/" . company-complete))
  :hook (after-init . company-tng-mode)
  :config
  (global-company-mode)
  (setq company-idle-delay 0.3 
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil))

(prelude-require-package 'company-quickhelp)
(use-package company-quickhelp
  :diminish
  :hook (global-company-mode . company-quickhelp-mode))

(prelude-require-package 'company-box)
(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil ;; TODO why?
        company-box-doc-delay 0.3))

(prelude-require-package 'ivy-prescient)
(use-package ivy-prescient
  :diminish
  :config
  (ivy-prescient-mode 1))

(prelude-require-package 'company-prescient)
(use-package company-prescient
  :diminish
  :config
  (company-prescient-mode 1))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :config
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
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

(prelude-require-package 'counsel-projectile)
(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(provide 'init-completion)

