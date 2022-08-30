;;; init-packages.el --- Package Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; TODO - straight.el?

(require 'package)
(require 'url)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(setq package-pinned-packages '((cider . "melpa-stable")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq package-native-compile t)

(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t
      use-package-always-defer t)

(use-package diminish)

(use-package epl
  :config
  ;; TODO make a copy of the built-in check for system packages work instead
  (defvar my/system-packages '(vterm))
  ;; emacs -Q --batch -L "~/.emacs.d/lisp/" -l "init-packages.el" -f "my/upgrade-packages"
  (defun my/upgrade-packages ()
    (interactive)
    (epl-refresh)
    (when-let ((upgrades (seq-filter (lambda (u)
                                       (not (member
                                             (epl-package-name (epl-upgrade-available u))
                                             my/system-packages)))
                                     (epl-find-upgrades))))
      ;; TODO why doesn't this work?
      ;; (epl-upgrade (mapcar 'epl-upgrade-available upgrades)
      (dolist (upgrade upgrades)
        (epl-package-install (epl-upgrade-available upgrade) 'force)
        (epl-package-delete (epl-upgrade-installed upgrade))))
    (message "Package upgrade finished.")
    ;; TODO does this help async native-comp to finish when called from the command line?
    (sit-for 30)))

(provide 'init-packages)
;;; init-packages.el ends here
