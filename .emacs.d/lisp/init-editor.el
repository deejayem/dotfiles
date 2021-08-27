;;; init-editor.el --- Editing Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; based on prelude-editor.el
;;; Code:

(use-package emacs
  :ensure nil
  :bind
  ("C-x \\" . align-regexp)
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  ;; (text-mode . whitespace-cleanup)

  :config
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)

  (setq-default indent-tabs-mode nil)
  (setq require-final-newline t)
  (setq comment-auto-fill-only-comments t)
  (setq large-file-warning-threshold 100000000)
  (setq create-lockfiles nil)
  (delete-selection-mode t)
  (global-auto-revert-mode t)

  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  (setq save-place-file (expand-file-name "saveplace" save-dir))
  (save-place-mode 1)

  ;; https://git.sr.ht/~technomancy/better-defaults/tree/master/item/better-defaults.el
  (setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        load-prefer-newer t)

  ;; https://github.com/natecox/dotfiles/blob/master/workspaces/shared/symlinks/emacs/.emacs.d/nathancox.org
  (setq sentence-end-double-space nil)
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (defadvice set-buffer-major-mode (after set-major-mode activate compile)
    "Set buffer major mode according to `auto-mode-alist'."
    (let* ((name (buffer-name buffer))
           (mode (assoc-default name auto-mode-alist 'string-match)))
      (when (and mode (consp mode))
        (setq mode (car mode)))
      (with-current-buffer buffer (if mode (funcall mode)))))

  (defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
    "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
    (ad-set-arg 0
                (mapcar (lambda (fn)
                          (let ((name (car fn)))
                            (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                                (cons
                                 (match-string 1 name)
                                 (cons (string-to-number (match-string 2 name))
                                       (string-to-number (or (match-string 3 name) ""))))
                              fn))) files)))

  (set-default 'imenu-auto-rescan t))

(use-package move-text
  :bind
  ("M-S-<up>" . move-text-up)
  ("M-S-<down>" . move-text-down))

(use-package zop-to-char
  :bind
  ("M-z" . zop-up-to-char)
  ("M-Z" . zop-to-char))

(use-package savehist
  :config
  (savehist-mode 1)
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" save-dir))
  :config
  (savehist-mode +1))

(use-package super-save
  :diminish
  :custom
  (super-save-remote-files nil)
  :config
  (super-save-mode +1)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(use-package recentf
  :config
  (recentf-mode +1)
  (add-to-list 'recentf-exclude (expand-file-name "elpa" user-emacs-directory))
  :custom
  (recentf-save-file (expand-file-name "recentf" save-dir))
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup (* 60 60)))

(use-package flycheck
  :config
  (global-flycheck-mode))

;(use-package flyspell
;  :custom
;  (ispell-program-name "aspell")
;  (ispell-extra-args '("--sug-mode=ultra"))
;  :hook
;  (text-mode . (lambda () (flyspell-mode +1)))
;  (prog-mode . (lambda () (flyspell-prog-mode))))

(use-package expand-region)

(use-package bookmark
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" save-dir))
  (bookmark-save-flag 1))

(use-package midnight)

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package undo-tree
  :diminish
  :after key-chord
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (undo-tree-auto-save-history t))

(use-package abbrev
  :ensure nil
  :diminish
  :hook (text-mode . abbrev-mode))

(use-package tabify
  :ensure nil
  :config
  (defmacro with-region-or-buffer (func)
    "When called with no active region, call FUNC on current buffer."
    `(defadvice ,func (before with-region-or-buffer activate compile)
       (interactive
        (if mark-active
            (list (region-beginning) (region-end))
          (list (point-min) (point-max))))))
  (with-region-or-buffer indent-region)
  (with-region-or-buffer untabify))

(use-package subword
  :diminish)

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;; (use-package adoc-mode
;;   (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
;;   (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode)))

(use-package operate-on-number
  :bind
  ("C-c ." . operate-on-number-at-point))

(provide 'init-editor)
;;; init-editor.el ends here
