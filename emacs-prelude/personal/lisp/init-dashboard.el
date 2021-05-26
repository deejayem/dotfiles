(prelude-require-package 'dashboard)
(use-package dashboard
  :init
  (setq dashboard-center-content t
        dashboard-set-footer nil
        dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)

