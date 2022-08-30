;;; init-web.el --- Web Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Some parts copied from prelude-web.el, prelude-js.el and prelude-css.el
;;; Code:

(use-package web-mode
  :defer 5
  :custom
  (web-mode-enable-auto-pairing nil)
  :config
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>"))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)))

(use-package css-mode
  :defer 5
  :custom
  (css-indent-offset 2)
  :config
  (rainbow-mode +1))

(use-package scss-mode
  :defer 5
  :config
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  :custom
  (scss-compile-at-save nil))

(use-package sass-mode
  :defer 5)

(use-package tagedit
  :defer 5
  :diminish
  :config (tagedit-add-paredit-like-keybindings)
  :hook (html-mode . (lambda () (tagedit-mode 1))))

(use-package js2-mode
  :defer 5
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :custom
  (js-indent-level 2)
  :hook (js2-mode . (lambda ()
                      (setq-local electric-layout-rules '((?\; . after)))
                      (setq mode-name "JS2")
                      (js2-imenu-extras-mode +1)
                      (subword-mode +1))))

(use-package mustache-mode
  :defer 5
  :config
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . mustache-mode)))

(provide 'init-web)
;;; init-web.el ends here
