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
  :diminish
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-autoskip-closing-pair 'always)
  (sp-hybrid-kill-entire-symbol t)
  (sp-hybrid-kill-excessive-whitespace nil)
  :hook (after-init . (lambda ()
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
  (unbind-key "M-?" 'smartparens-mode-map)
  (unbind-key "M-?" 'sp-keymap)
  :bind (:map smartparens-mode-map
              ("C-M-?" . sp-convolute-sexp)
              ([remap mark-sexp] . sp-mark-sexp)
              ("M-[" . sp-wrap-square)
              ("C-c M-{" . sp-wrap-curly)
              ("M-W" . sp-copy-sexp)
              (")" . paredit-close-round)
              ("]" . paredit-close-square)
              ("}" . paredit-close-curly)
              (";" . paredit-semicolon)
              ("M-;" . paredit-comment-dwim)
              ("M-q" . sp-indent-defun)
              ("C-j" . sp-newline)
              ("C-c C-S-d" . duplicate-sexp-after-point)
              ("C-c M-(" . wrap-round-from-behind)))

(provide 'init-smartparens)
;;; init-smartparens.el ends here
