(prelude-require-package 'dashboard)
(use-package dashboard
  :init
  (setq dashboard-center-content t
        dashboard-startup-banner 'logo
        dashboard-set-footer nil
        dashboard-week-agenda t
        dashboard-projects-backend 'projectile
        ;dashboard-projects-switch-function 'projectile-persp-switch-project
        dashboard-items '((recents  . 15)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
