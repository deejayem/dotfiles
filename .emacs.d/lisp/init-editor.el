;;; init-editor.el --- Editing Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; based on prelude-editor.el
;;; Code:

(use-package emacs
  :bind
  ("C-x \\" . align-regexp)
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (emacs-startup . (lambda ()
                     (save-place-mode 1)
                     (delete-selection-mode t)
                     (global-auto-revert-mode 1)
                     (set-terminal-coding-system 'utf-8)
                     (set-keyboard-coding-system 'utf-8)
                     (set-selection-coding-system 'utf-8)
                     (prefer-coding-system 'utf-8)))
  ;; (text-mode . whitespace-cleanup)

  :config
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)

  (setq-default indent-tabs-mode nil
                tab-width 4)
  (setq comment-auto-fill-only-comments t)
  (setq large-file-warning-threshold 100000000)
  (setq create-lockfiles nil)
  (setq global-auto-revert-non-file-buffers t)
  (setq backup-by-copying t)
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  (setq save-place-file (expand-file-name "saveplace" save-dir))
  ;; https://git.sr.ht/~technomancy/better-defaults/tree/master/item/better-defaults.el
  (setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t)

  (setq ffap-machine-p-known 'reject)

  ;; https://github.com/natecox/dotfiles/blob/master/workspaces/shared/symlinks/emacs/.emacs.d/nathancox.org
  (setq sentence-end-double-space nil)
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8)
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
  (add-to-list 'recentf-exclude (expand-file-name "straight" user-emacs-directory))
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
  (undo-tree-enable-undo-in-region t)
  (undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (undo-tree-auto-save-history t))

(use-feature abbrev
  :defer 5
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

(use-feature xref
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

(use-package ws-butler
  :diminish
  :hook (prog-mode . ws-butler-mode))

(use-package aggressive-indent
  :diminish
  :config
  (unbind-key "C-c C-q" aggressive-indent-mode-map)
  ;; don't indent lisp comments
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'prog-mode)
                     (string-match "^\s*;"
                                   (or (thing-at-point 'line) ""))))
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package hungry-delete
  :diminish
  :hook (prog-mode . turn-on-hungry-delete-mode))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package auto-yasnippet
  :bind
  ("C-c C-y w" . aya-create)
  ("C-c C-y TAB" . aya-expand)
  ("C-c C-y SPC" . aya-expand-from-history)
  ("C-c C-y d" . aya-delete-from-history)
  ("C-c C-y c" . aya-clear-history)
  ("C-c C-y n" . aya-next-in-history)
  ("C-c C-y p" . aya-previous-in-history)
  ("C-c C-y s" . aya-persist-snippet)
  ("C-c C-y o" . aya-open-line))

(use-package editorconfig
  :diminish
  :custom (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :hook (emacs-startup . editorconfig-mode))

(provide 'init-editor)
;;; init-editor.el ends here
