;;; init-navigation.el --- Navigation Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar-local goto-char--last-char nil)
(use-package emacs
  :config
  (defun goto-char-forward (arg char)
    "Move forward to char in line.
If a C-u prefix argument is given, it is not restricted to the current line.
If a numeric prefix argument N is given, move forward N instances of char."
    (interactive "P\ncGo to char: ")
    (setq goto-char--last-char char)
    (goto-char--move-forward arg))

  (defun goto-char-forward-repeat-last (arg)
    "Move forward in line to char last used in a goto-char command.
If a C-u prefix argument is given, it is not restricted to the current line.
If a numeric prefix argument N is given, move forward N instances of the last
used char."
    (interactive "P")
    (goto-char--move-forward arg))

  (defun goto-char-backward (arg char)
    "Move forward to char in line.
If a C-u prefix argument is given, it is not restricted to the current line.
If a numeric prefix argument N is given, move back N instances of char."
    (interactive "P\ncGo to char (backward):")
    (setq goto-char--last-char char)
    (goto-char--move-backward arg))

  (defun goto-char-backward-repeat-last (arg)
    "Move backward in line to char last qused in a goto-char command.
If a C-u prefix argument is given, it is not restricted to the current line.
If a numeric prefix argument N is given, move back N instances of the last used
char."
    (interactive "P")
    (goto-char--move-backward arg))

  (defun goto-char--move-forward (arg)
    (when goto-char--last-char
      (let ((count (if (consp arg)
                       2 ;; C-u -> 2 (i.e. first match on any line)
                       (if (equal (char-after) goto-char--last-char)
                           (1+ (or arg 1)) ;; skip over the char after the cursor, if needed
                         arg)))
            (end-position (unless (consp arg)
                            (line-end-position))))
        (when (search-forward (string goto-char--last-char) end-position t count)
          (backward-char)))))

  (defun goto-char--move-backward (arg)
    (when goto-char--last-char
      (let ((count (unless (consp arg)
                     arg))
            (start-position (unless (consp arg)
                              (line-beginning-position))))
        (search-backward (string goto-char--last-char) start-position t count))))

  :bind
  ("C-'" . goto-char-forward)
  ("C-;" . goto-char-backward)
  ("C-@" . goto-char-forward-repeat-last)
  ("C-:" . goto-char-backward-repeat-last))

(use-package smartscan
  :config
  (global-smartscan-mode t)
  :hook
  (cider-repl-mode . (lambda () (smartscan-mode -1)))
  (ielm-mode . (lambda () (smartscan-mode -1)))
  (vterm-mode . (lambda () (smartscan-mode -1)))
  (eshell-mode . (lambda () (smartscan-mode -1))))

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

(provide 'init-navigation)
;;; init-navigation.el ends here
