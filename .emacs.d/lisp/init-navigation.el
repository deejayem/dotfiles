;;; init-navigation.el --- Navigation Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package avy
  :config
  ;; https://karthinks.com/software/avy-can-do-anything/#avy-plus-embark-any-action-anywhere
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (add-to-list 'avy-dispatch-alist '(111 . avy-action-embark))
  :bind
  ("C-'" . avy-goto-char-timer)
  ("C-;" . avy-goto-char-in-line))

(use-package smartscan
  :config
  (global-smartscan-mode t)
  :hook
  (cider-repl-mode . (lambda () (smartscan-mode -1)))
  (ielm-mode . (lambda () (smartscan-mode -1)))
  (vterm-mode . (lambda () (smartscan-mode -1)))
  (eshell-mode . (lambda () (smartscan-mode -1)))
  (sql-interactive-mode . (lambda () (smartscan-mode -1))))

(use-package symbol-overlay
  :bind
  ("C-c o" . symbol-overlay-put)
  ("M-N" . symbol-overlay-switch-forward)
  ("M-P" . symbol-overlay-switch-backward)
  ("<f8>" . symbol-overlay-remove-all))

(use-package affe
  :after (consult orderless)
  :config
  (setq affe-grep-command (replace-regexp-in-string "\\." "-Suu ." affe-grep-command))
  ;; Configure Orderless
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))
  (defun my/affe-grep-symbol-at-point (&optional dir initial)
    (interactive
     (list prefix-arg (when-let ((s (symbol-at-point)))
                        (symbol-name s))))
    (affe-grep dir initial))
  (defun my/affe-find-symbol-at-point (&optional dir initial)
    (interactive
     (list prefix-arg (when-let ((s (symbol-at-point)))
                        (symbol-name s))))
    (affe-find dir initial))
  :bind
  ("C-#" . affe-grep)
  ("C-c z" . affe-find)
  ("C-c Z" . my/affe-find-symbol-at-point)
  ("C-~" . my/affe-grep-symbol-at-point))

(use-package rg
  :bind
  ("C-c C-M-r" . rg-menu)
  ("C-c C-M-R" . rg))

(use-package gumshoe
  :defer 5
  :after perspective
  :diminish global-gumshoe-persp-mode
  :custom (gumshoe-show-footprints-p nil)
  :config
  (global-gumshoe-persp-mode +1)
  (defvar gumshoe-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "]") #'gumshoe-buf-backtrack-forward)
      (define-key map (kbd "[") #'gumshoe-buf-backtrack-back)
      (define-key map (kbd "}") #'gumshoe-persp-backtrack-forward)
      (define-key map (kbd "{") #'gumshoe-persp-backtrack-back)
      map))
  (dolist (cmd '(gumshoe-buf-backtrack-forward gumshoe-buf-backtrack-back gumshoe-persp-backtrack-forward gumshoe-persp-backtrack-back))
    (put cmd 'repeat-map 'gumshoe-repeat-map))
  :bind
  ("C-c ]" . gumshoe-buf-backtrack-forward)
  ("C-c [" . gumshoe-buf-backtrack-back)
  ("C-c }" . gumshoe-persp-backtrack-forward)
  ("C-c {" . gumshoe-persp-backtrack-back)
  ("C-c '" . gumshoe-peruse-in-persp))

(provide 'init-navigation)
;;; init-navigation.el ends here
