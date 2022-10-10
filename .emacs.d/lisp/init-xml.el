;;; init-xml.el --- XML Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Based on prelude-xml.el
;;; Code:

(use-feature nxml-mode
  :config
  (push '("<\\?xml" . nxml-mode) magic-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.pom\\'" . nxml-mode))
  :custom
  (nxml-child-indent 4)
  (nxml-attribute-indent 4)
  (nxml-auto-insert-xml-declaration-flag nil)
  (nxml-bind-meta-tab-to-complete-flag t)
  (nxml-slash-auto-complete-flag t))

(provide 'init-xml)
;;; init-xml.el ends here
