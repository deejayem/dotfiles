;;; init-dashboard.el --- Dashboard Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dashboard
  :init
  (setq dashboard-center-content t
        dashboard-startup-banner 'logo
        dashboard-set-footer nil
        dashboard-week-agenda t
        dashboard-projects-backend 'project-el
        dashboard-projects-switch-function 'switch-project
        dashboard-items '((recents  . 15)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
