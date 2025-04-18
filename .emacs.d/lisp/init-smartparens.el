;;; init-smartparens.el --- Smartparens Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package paredit
  :commands (paredit-semicolon
             paredit-comment-dwim
             paredit-close-round
             paredit-close-square
             paredit-close-curly))

(use-package smartparens
  ;; Use this fork until it is merged
  :ensure (:host github :repo "thatismatt/smartparens")
  :diminish
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-autoskip-closing-pair 'always)
  (sp-hybrid-kill-entire-symbol t)
  (sp-hybrid-kill-excessive-whitespace nil)
  :hook (elpaca-after-init . (lambda ()
                               (smartparens-global-strict-mode)
                               (show-smartparens-global-mode)
                               (setq sp-paredit-bindings (delete '("M-?" . sp-convolute-sexp) sp-paredit-bindings))
                               (require 'smartparens-config)
                               (sp-use-paredit-bindings)))
  :config
  (sp-pair "\"" "\"" :wrap "M-\"")
  ;; From https://github.com/bodil/emacs.d/blob/master/bodil/bodil-paredit.el
  (defun duplicate-sexp-after-point ()
    "Duplicates the content of the line that is after the point."
    (interactive)
    ;; skips to the next sexp
    (while (looking-at " ")
      (forward-char))
    (set-mark-command nil)
    ;; while we find sexps we move forward on the line
    (while (and (<= (point) (car (bounds-of-thing-at-point 'sexp)))
                (not (= (point) (line-end-position))))
      (forward-sexp)
      (while (looking-at " ")
        (forward-char)))
    (kill-ring-save (mark) (point))
    ;; go to the next line and copy the sexps we encountered
    (sp-newline)
    (set-mark-command nil)
    (yank)
    (exchange-point-and-mark))
  ;; From https://github.com/bodil/emacs.d/blob/master/bodil/bodil-paredit.el
  ;; Inverse M-(
  (defun wrap-round-from-behind ()
    "Wrap the previous sexp before the point with ()."
    (interactive)
    (forward-sexp -1)
    (sp-wrap-round)
    (insert " ")
    (forward-char -1))
  (defun kill-around-sexp ()
    "Kill everything in the current list, except the current expression.
Equivalent to raising then wrapping."
    (interactive)
    (let ((paren-char (save-excursion
                        (sp-backward-up-sexp)
                        (following-char))))
      (sp-raise-sexp)
      (cond ((= paren-char ?\() (sp-wrap-round))
            ((= paren-char ?\{) (sp-wrap-curly))
            ((= paren-char ?\[) (sp-wrap-square))
            (t (error "Not in a list")))))
  (defun sp-mark-sexp-advice (origin &rest args)
    (if (sp--raw-argument-p (car args))
        (push-mark
         (save-excursion
           (sp-end-of-sexp)
           (point))
         nil t)
      (apply origin args)))
  (advice-add 'sp-mark-sexp :around 'sp-mark-sexp-advice)
  (defun sp-fwd-sexp (arg)
    (interactive "^P")
    (if (sp--raw-argument-p arg)
        (sp-end-of-sexp)
      (call-interactively 'sp-forward-sexp arg)))
  (defun sp-bwd-sexp (arg)
    (interactive "^P")
    (if (sp--raw-argument-p arg)
        (sp-beginning-of-sexp)
      (call-interactively 'sp-backward-sexp arg)))
  (unbind-key "M-?" 'smartparens-mode-map)
  (unbind-key "M-?" 'sp-keymap)
  :bind (:map smartparens-mode-map
              ("C-M-?" . sp-convolute-sexp)
              ([remap mark-sexp] . sp-mark-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("M-K" . kill-sexp)
              ([remap sp-forward-sexp] . sp-fwd-sexp)
              ([remap sp-backward-sexp] . sp-bwd-sexp)
              ("M-[" . sp-wrap-square)
              ("C-M-{" . sp-wrap-curly)
              ("M-W" . sp-copy-sexp)
              (")" . paredit-close-round)
              ("]" . paredit-close-square)
              ("}" . paredit-close-curly)
              (";" . paredit-semicolon)
              ("M-;" . paredit-comment-dwim)
              ("M-q" . sp-indent-defun)
              ("C-j" . sp-newline)
              ("M-R" . kill-around-sexp)
              ("M-D" . sp-unwrap-sexp)
              ("C-M-S-d" . sp-backward-unwrap-sexp)
              ("C-c C-S-d" . duplicate-sexp-after-point)
              ("C-c M-(" . wrap-round-from-behind)))

(provide 'init-smartparens)
;;; init-smartparens.el ends here
