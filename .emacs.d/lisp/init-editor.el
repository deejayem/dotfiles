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
  (setq comment-auto-fill-only-comments t)
  (setq large-file-warning-threshold 100000000)
  (setq create-lockfiles nil)
  (delete-selection-mode t)
  (setq global-auto-revert-non-file-buffers t)
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
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" save-dir))
  :hook (after-init . savehist-mode))

(use-package super-save
  :defer 5
  :diminish
  :custom
  (super-save-remote-files nil)
  :config
  (super-save-mode +1)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude (expand-file-name "elpa" user-emacs-directory))
  :custom
  (recentf-save-file (expand-file-name "recentf" save-dir))
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup (* 60 60))
  :hook (after-init . recentf-mode))

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

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package bookmark
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" save-dir))
  (bookmark-save-flag 1))

(use-package midnight)

(use-package undo-tree
  :defer 5
  :diminish
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (undo-tree-auto-save-history t))

(use-package abbrev
  :defer 5
  :ensure nil
  :diminish
  :hook
  (text-mode . abbrev-mode)
  (prog-mode . abbrev-mode)
  (cider-repl-mode . abbrev-mode))

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

(use-package xref
  :ensure nil
  :custom (xref-search-program 'ripgrep)
  :config
  (defun xref-find-references-other-window (identifier)
    "Like `xref-find-references' but switch to the other window"
    (interactive (list (xref--read-identifier "Find references of: ")))
      (xref--find-xrefs identifier 'references identifier 'window))
  (defun xref-find-references-other-frame (identifier)
    "Like `xref-find-references' but switch to the other frame"
    (interactive (list (xref--read-identifier "Find references of: ")))
    (xref--find-xrefs identifier 'references identifier 'frame))
  (define-key ctl-x-4-map (kbd "M-?") 'xref-find-references-other-window)
  (define-key ctl-x-5-map (kbd "M-?") 'xref-find-references-other-window)
  ;; 'xref-prompt-for-identifier begins with not, so adding this prevents
  ;; prompting for an identifier when calling xref-find-references, unless
  ;; there is no value at point that can be used
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references-other-window t)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references-other-frame t))

(provide 'init-editor)
;;; init-editor.el ends here
