;;; init-editor.el --- Editing Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; based on prelude-editor.el
;;; Code:

(use-feature emacs
  :bind
  ("C-x \\" . align-regexp)
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (elpaca-after-init . (lambda ()
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
  (setq global-auto-revert-non-file-buffers t
        revert-without-query (list ".")
        auto-revert-stop-on-user-input nil)
  (setq backup-by-copying t
        delete-old-versions t
        version-control t
        kept-new-versions 5
        kept-old-versions 5)
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))
        auto-save-include-big-deletions t)
  (setq comment-multi-line t
        comment-empty-lines t)

  (setq save-place-file (expand-file-name "saveplace" save-dir)
        save-place-limit 800)
  ;; https://git.sr.ht/~technomancy/better-defaults/tree/master/item/better-defaults.el
  (setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t)

  (setq ffap-machine-p-local 'accept
        ffap-machine-p-known 'reject
        ffap-machine-p-unkown 'reject)

  (setq sentence-end-double-space nil)
  (set-language-environment "UTF-8")
  ;; The previous line sets this to "rfc1345"
  (setq default-input-method nil)

  (set-default 'imenu-auto-rescan t))

(use-package move-text
  :bind
  ("M-S-<up>" . move-text-up)
  ("M-S-<down>" . move-text-down))

(use-package zop-to-char
  :bind
  ("M-z" . zop-up-to-char)
  ("M-Z" . zop-to-char))

(use-feature savehist
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" save-dir))
  (history-length 300)
  :hook (elpaca-after-init . savehist-mode))

(use-package super-save
  :defer 5
  :diminish
  :custom
  (super-save-remote-files nil)
  :config
  (super-save-mode +1)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(use-feature recentf
  :config
  (add-to-list 'recentf-exclude (expand-file-name "elpa" user-emacs-directory))
  (add-to-list 'recentf-exclude (expand-file-name "straight" user-emacs-directory))
  :custom
  (recentf-save-file (expand-file-name "recentf" save-dir))
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup (* 60 60))
  :hook (elpaca-after-init . recentf-mode))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-feature flyspell
  :diminish
  :config
  (when (string-suffix-p "aspell" ispell-program-name)
    (setq ispell-extra-args '("--sug-mode=ultra")))
  (unbind-key "C-," flyspell-mode-map)
  (unbind-key "C-." flyspell-mode-map)
  ;;(unbind-key "C-;" flyspell-mode-map)
  :custom (flyspell-auto-correct-binding (kbd "C-x C-M-;"))
  :bind (:map flyspell-mode-map
              ("C-x C-," . flyspell-goto-next-error)
              ("C-x C-." . flyspell-correct-word))
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(use-feature bookmark
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" save-dir))
  (bookmark-save-flag 1))

(use-feature midnight)

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
  :diminish
  :hook
  (text-mode . abbrev-mode)
  (prog-mode . abbrev-mode)
  (cider-repl-mode . abbrev-mode))

(use-feature subword
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
  ("C-c +" . operate-on-number-at-point))

(defun +elpaca-unload-xref (e)
  (and (featurep 'xref) (unload-feature 'xref t))
  ;; Make sure xref-find-definitions doesn't override this embark binding (unless https://github.com/oantolin/embark/issues/732 can be fixed)
  (bind-key "M-." 'embark-dwim)
  ;; Make sure these aren't overwritten
  (setq xref-search-program 'ripgrep
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (elpaca--continue-build e))

(defun +elpaca-xref-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "xref" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-xref 'elpaca--activate-package)))

(use-package xref
  :ensure `(xref :build ,(+elpaca-xref-build-steps) :ref "87db670d045bea2d90139b1f741eea8db7c193ea" :pin t)
  :custom
  (xref-search-program 'ripgrep)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (defun xref-find-references-current-defun ()
    "`xref-find-references' for the enclosing defun."
    (interactive)
    (xref-backend-identifier-completion-table (xref-find-backend))
    (xref-find-references (which-function)))
  (defun xref-find-definitions-current-list-function ()
    "`xref-find-definitions' for the function at the beginning of the current list.
With a prefix argument, moves up `current-prefix-arg' sexps first."
    (interactive)
    (let ((fn-name (save-excursion
                     (when current-prefix-arg
                       (sp-backward-up-sexp current-prefix-arg))
                     (sp-beginning-of-sexp) (thing-at-point 'symbol))))
      (xref-find-definitions fn-name)))
  (defun xref-find-references-other-window (identifier)
    "Like `xref-find-references' but switch to the other window."
    (interactive (list (xref--read-identifier "Find references of: ")))
    (xref--find-xrefs identifier 'references identifier 'window))
  (defun xref-find-references-other-frame (identifier)
    "Like `xref-find-references' but switch to the other frame."
    (interactive (list (xref--read-identifier "Find references of: ")))
    (xref--find-xrefs identifier 'references identifier 'frame))
  (define-key ctl-x-4-map (kbd "M-?") 'xref-find-references-other-window)
  (define-key ctl-x-5-map (kbd "M-?") 'xref-find-references-other-window)
  ;; 'xref-prompt-for-identifier begins with not, so adding this prevents
  ;; prompting for an identifier when calling xref-find-references, unless
  ;; there is no value at point that can be used
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references-other-window t)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references-other-frame t)
  :bind
  ("C-c q" . xref-find-references-current-defun)
  ("C-c C-M-." . xref-find-definitions-current-list-function)
  ;; Make sure xref-find-definitions doesn't override this embark binding (unless https://github.com/oantolin/embark/issues/732 can be fixed)
  ("M-." . embark-dwim))

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
  :hook (elpaca-after-init . editorconfig-mode))

(use-package titlecase
  :bind (("C-c c t t" . titlecase-dwim)
         (:map embark-heading-map ("T" . titlecase-line))
         (:map embark-region-map ("T" . titlecase-region))))

(use-package caser
  :ensure (caser :host github :repo "emacsmirror/caser")
  :bind
  ("C-c c c" . caser-camelcase-dwim)
  ("C-c c s" . caser-snakecase-dwim)
  ("C-c c d" . caser-dashcase-dwim))

(use-package selected
  :defer 6
  :diminish selected-minor-mode
  :config
  (setq selected-emacs-lisp-mode-map (make-sparse-keymap)
        selected-org-mode-map (make-sparse-keymap)
        selected-clojure-mode-map (make-sparse-keymap))
  (add-to-list 'selected-ignore-modes 'magit-status-mode)
  (selected-global-mode +1)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("l" . downcase-region)
              ("C" . count-words-region)
              ("m" . apply-macro-to-region-lines)
              ("c" . copy-region-as-kill)
              ("M-d" . sp-delete-region)
              ("j" . jq-format-json-region)
              ("i" . indent-region)
              ("C-d" . duplicate-dwim)
              ("n" . narrow-to-region)
              ("_" . ws-butler-clean-region)
              ("t t" . titlecase-dwim)
              ("t c" . caser-camelcase-dwim)
              ("t s" . caser-snakecase-dwim)
              ("t d" . caser-dashcase-dwim)
              :map selected-org-mode-map
              ("t" . org-table-convert-region)
              :map selected-emacs-lisp-mode-map
              ("x" . elisp-eval-region-or-buffer)
              :map selected-clojure-mode-map
              ("x" . cider-eval-region)
              ("r" . cider-insert-region-in-repl)))

(provide 'init-editor)
;;; init-editor.el ends here
