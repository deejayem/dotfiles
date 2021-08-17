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

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

(use-package diminish)

(use-package paradox
  :config
  (paradox-enable))

(defvar vertico-extensions-dir (expand-file-name "site-lisp/vertico-extensions" user-emacs-directory))
(defvar vertico-extensions '("vertico-directory" "vertico-repeat"))

(defun fetch-vertico-extensions ()
  "Download the latest versions of the required vertico extensions into vertico-extensions-dir"
  (dolist (extension vertico-extensions)
    (let ((ext-file (format "%s.el" extension)))
      (url-copy-file
       (format "https://raw.githubusercontent.com/minad/vertico/main/extensions/%s" ext-file)
       (expand-file-name ext-file vertico-extensions-dir)))))

(unless (file-directory-p vertico-extensions-dir)
  (make-directory vertico-extensions-dir t)
  (fetch-vertico-extensions))

(use-package epl
  :config
  (defun my/upgrade-packages ()
    (interactive)
    (epl-refresh)
    (epl-upgrade)
    (fetch-vertico-extensions)
    (message "Package upgrade finished.")))

(provide 'init-packages)
;;; init-packages.el ends here
