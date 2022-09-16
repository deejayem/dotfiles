;;; init-web.el --- Web Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Some parts copied from prelude-web.el, prelude-js.el and prelude-css.el
;;; Code:

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
  :init
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package css-mode
  :custom
  (css-indent-offset 2)
  :config
  (rainbow-mode +1))

(use-package scss-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  :custom
  (scss-compile-at-save nil))

(use-package sass-mode)

(use-package tagedit
  :diminish
  :commands tagedit-mode
  :config (tagedit-add-paredit-like-keybindings)
  :hook (html-mode . (lambda () (tagedit-mode 1))))

(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :custom
  (js-indent-level 2)
  :hook (js2-mode . (lambda ()
                      (setq-local electric-layout-rules '((?\; . after)))
                      (setq mode-name "JS2")
                      (js2-imenu-extras-mode +1)
                      (subword-mode +1))))

(use-package mustache-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . mustache-mode)))

(provide 'init-web)
;;; init-web.el ends here
