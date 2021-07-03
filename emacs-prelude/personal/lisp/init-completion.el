(setq tab-always-indent 'complete)

(use-package company
  :bind (("M-/" . company-complete))
  :hook (after-init . company-tng-mode)
  :config
  (global-company-mode)
  (setq ;company-idle-delay 0.3
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
        ;company-box-doc-delay 0.3
	))

(prelude-require-package 'company-prescient)
(use-package company-prescient
  :diminish
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode +1))

(provide 'init-completion)

