;;; init-navigation.el --- Navigation Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-timeout-seconds 0.3)
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
  (defun avy-copy-as-kill ()
    (interactive)
    (avy-goto-char-timer)
    (let ((beg (point)))
      (avy-goto-char-timer)
      (copy-region-as-kill beg (point))))
  (defun avy-kill-in-line ()
    (interactive)
    (avy-goto-char-timer)
    (let ((beg (point)))
      (call-interactively 'avy-goto-char-in-line)
      (copy-region-as-kill beg (point))))
  :bind
  ("C-'" . avy-goto-char-timer)
  ("C-;" . avy-goto-char-in-line)
  ("C-c C-'" . avy-copy-as-kill)
  ("C-c C-;" . avy-copy-as-kill-in-line))

(use-package smartscan
  :config
  (unbind-key "M-'" smartscan-map)
  (defvar-local smartscan-exclude-modes '(cider-repl-mode
                                          ielm-mode
                                          vterm-mode
                                          term-mode
                                          ansi-term-mode
                                          eshell-mode
                                          shell-mode
                                          sql-interactive-mode
                                          magit-status-mode
                                          compilation-mode
                                          deadgrep-mode))
  (defun turn-off-smartscan-mode ()
    (smartscan-mode -1))
  (dolist (mode smartscan-exclude-modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'turn-off-smartscan-mode))
  :hook
  (after-init . global-smartscan-mode)
  :bind (:map smartscan-map
              ("C-M-'" . smartscan-symbol-replace)))

(use-package symbol-overlay
  :config
  (defun symbol-overlay-put-or-clear (arg)
    "Toggle all overlays of symbol at point.
Or remove all highlighted symbols in the current buffer (with`ARG')."
    (interactive "P")
    (if arg
        (symbol-overlay-remove-all)
      (symbol-overlay-put)))
  :bind
  ("C-c o" . symbol-overlay-put-or-clear)
  ("M-N" . symbol-overlay-switch-forward)
  ("M-P" . symbol-overlay-switch-backward))

(use-package gumshoe
  :defer 5
  :after perspective
  :diminish global-gumshoe-mode
  :custom
  (gumshoe-show-footprints-p nil)
  (gumshoe-idle-time 5)
  (gumshoe-follow-distance 5)
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

(use-package goto-chg
  :config
  (defvar goto-chg-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-(") #'goto-last-change)
      (define-key map (kbd "C-)") #'goto-last-change-reverse)
      map))
  (dolist (cmd '(goto-last-change goto-last-change-reverse))
    (put cmd 'repeat-map 'goto-chg-repeat-map))
  :bind
  ("C-c C-(" . goto-last-change)
  ("C-c C-)" . goto-last-change-reverse))

(use-package goto-last-point
  :diminish
  :custom (goto-last-point-max-length 100)
  :hook (emacs-startup . goto-last-point-mode)
  :config
  (defvar goto-last-point-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<") #'goto-last-point)
      map))
  (put 'goto-last-point 'repeat-map 'goto-last-point-repeat-map)
  :bind ("C-c <" . goto-last-point))

(use-package link-hint
  :bind
  ("C-c C-l" . link-hint-open-link)
  ("C-c C-S-l" . link-hint-copy-link))

(provide 'init-navigation)
;;; init-navigation.el ends here
