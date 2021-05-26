;(prelude-require-package 'all-the-icons)

(toggle-frame-maximized)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq whitespace-line-column 120)

(use-package paren
  :config
  (show-paren-mode +1))

(provide 'init-ui)

