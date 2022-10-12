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
  (defvar-local smartscan-exclude-modes '(cider-repl-mode
                                          ielm-mode
                                          vterm-mode
                                          term-mode
                                          ansi-term-mode
                                          eshell-mode
                                          shell-mode
                                          sql-interactive-mode
                                          compilation-mode
                                          deadgrep-mode))
  (defun turn-off-smartscan-mode ()
    (smartscan-mode -1))
  (dolist (mode smartscan-exclude-modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'turn-off-smartscan-mode))
  :hook
  (after-init . global-smartscan-mode))

(use-package symbol-overlay
  :bind
  ("C-c o" . symbol-overlay-put)
  ("M-N" . symbol-overlay-switch-forward)
  ("M-P" . symbol-overlay-switch-backward)
  ("<f8>" . symbol-overlay-remove-all))

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
