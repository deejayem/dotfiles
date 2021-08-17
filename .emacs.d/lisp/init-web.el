;;; init-ui.el --- Web Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Some parts copied from prelude-web.el, prelude-js.el and prelude-css.el
;;; Code:

(use-package css-mode
  :custom
  (css-indent-offset 2)
  :config
  (rainbow-mode +1))

(use-package sass-mode)

(use-package web-mode
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

(use-package tagedit
  :config (tagedit-add-paredit-like-keybindings)
  :hook (html-mode . (lambda () (tagedit-mode 1))))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
  :hook (js2-mode . (lambda ()
                      (setq-local electric-layout-rules '((?\; . after)))
                      (setq mode-name "JS2")
                      (js2-imenu-extras-mode +1)
                      (subword-mode +1))))

(use-package scss-mode
 :custom
 (scss-compile-at-save nil))

(provide 'init-web)
;;; init-web.el ends here
