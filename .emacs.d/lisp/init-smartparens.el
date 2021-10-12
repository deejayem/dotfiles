;;; init-smartparens.el --- Smartparens Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package smartparens
  :diminish
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-autoskip-closing-pair 'always)
  (sp-hybrid-kill-entire-symbol t)
  (sp-hybrid-kill-excessive-whitespace t)
  :config
  (defun sp-wrap-double-quotation-marks ()
    (interactive)
    (sp-wrap-with-pair "\""))
  ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+spacemacs/spacemacs-editing/funcs.el (spacemacs/smart-closing-parenthesis)
  ;; TODO can make things unbalanced
  (defun sp-close-round ()
    (interactive)
    (let* ((sp-navigate-close-if-unbalanced t)
           (current-pos (point))
           (current-line (line-number-at-pos current-pos))
           next-pos next-line)
      (save-excursion
        (let ((buffer-undo-list)
              (modified (buffer-modified-p)))
          (unwind-protect
              (progn
                (sp-up-sexp)
                (setq next-pos (point)
                      next-line (line-number-at-pos)))
            (primitive-undo (length buffer-undo-list)
                            buffer-undo-list)
            (set-buffer-modified-p modified))))
      (cond
       ((and (= current-line next-line)
             (not (= current-pos next-pos)))
        (sp-up-sexp))
       (t
        (insert-char ?\))))))

  (smartparens-global-strict-mode)
  (show-smartparens-global-mode)
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (unbind-key "M-?" 'smartparens-mode-map)
  :commands sp-local-pair
  :bind (:map smartparens-mode-map
              ("C-M-?" . sp-convolute-sexp)
              ;; (")" . sp-close-round)
              ("M-\"" . sp-wrap-double-quotation-marks)))

(provide 'init-smartparens)
;;; init-smartparens.el ends here
