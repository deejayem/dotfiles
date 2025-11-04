;;; init-shell.el --- eshell/vterm Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-feature eshell
  :bind ("C-x m " . eshell)
  :hook
  (eshell-pre-command . eshell-save-some-history)
  (eshell-mode-hook . (lambda () (setenv "TERM" "xterm-256color")))
  :custom
  (eshell-directory-name (expand-file-name "eshell" save-dir))
  ;; https://lambdaland.org/posts/2024-08-19_fancy_eshell_prompt/#eshell-prompt
  ;; TODO fix this
  ;;(eshell-highlight-prompt nil)
  (eshell-prompt-regexp "^[^#$\n]* [$#] ")
  (eshell-prompt-function
   (lambda ()
     (let* ((cwd (abbreviate-file-name (eshell/pwd)))
            (ref (magit-get-shortname "HEAD"))
            (stat (magit-file-status))
            (x-stat eshell-last-command-status)
            (git-chunk
             (if ref
                 (format "%s%s%s "
                         (propertize (if stat "[" "(") 'font-lock-face (list :foreground (if stat "red" "green")))
                         (propertize ref 'font-lock-face '(:foreground "yellow"))
                         (propertize (if stat "]" ")") 'font-lock-face (list :foreground (if stat "red" "green"))))
               "")))
       (propertize
        (format "%s %s %s$ "
                (if (< 0 x-stat) (format (propertize "!%s" 'font-lock-face '(:foreground "red")) x-stat)
                  (propertize "➤" 'font-lock-face (list :foreground (if (< 0 x-stat) "red" "green"))))
                (propertize cwd 'font-lock-face '(:foreground "#45babf"))
                git-chunk)
        'read-only t
        'front-sticky   '(font-lock-face read-only)
        'rear-nonsticky '(font-lock-face read-only)))))
  :config
  (setenv "PAGER" "cat"))

(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package xterm-color
  :after esh-mode
  :hook
  (eshell-before-prompt . (lambda ()
                            (setq xterm-color-preserve-properties t)))
  :config
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)
  (setenv "TERM" "xterm-256color"))

;; Installed with home-manager
(use-feature multi-vterm
  :bind (("C-c t" . multi-vterm-next)
         ("C-x p t" . multi-vterm-project)
         ("C-c C-M-t" . multi-vterm)
         (:map vterm-mode-map
               ("M-[" . multi-vterm-prev)
               ("M-]" . multi-vterm-next))))

(provide 'init-shell)
;;; init-shell.el ends here
