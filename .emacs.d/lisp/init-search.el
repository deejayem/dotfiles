;;; init-search.el --- Search Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-feature isearch
  :config
  (defface isearch-prompt
    '((t (:foreground "gold")))
    "Face for isearch minibuffer prompt."
    :group 'isearch)
  ;; https://www.emacs.dyerdwelling.family/emacs/20230503211610-emacs--isearch-occur-advice-window-focus/
  (defun isearch-occur-advice (origin &rest args)
    (isearch-exit)
    (select-window (get-buffer-window "*Occur*"))
    (goto-char (point-min)))
  (advice-add 'isearch-occur :after 'isearch-occur-advice)

  :custom
  (search-whitespace-regexp ".*\\b")
  (isearch-lax-whitespace t)
  (isearch-allow-scroll t)
  (isearch-yank-on-move 'shift)
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format "   (%s/%s)")
  (lazy-highlight-initial-delay 0)
  (isearch-message-properties '(read-only t cursor-intangible t face isearch-prompt))
  :bind-keymap ("C-c s" . search-map) ;; M-s clashes with paredit/smartparens bindings
  :bind
  (:map search-map
        ("<" . isearch-beginning-of-buffer)
        (">" . isearch-end-of-buffer)))

(use-feature casual-isearch
  :after isearch
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu))
  :config
  ;; Replace isearch-query-replace functions with their aznu equivalents
  (transient-replace-suffix 'casual-isearch-tmenu "r" '("r" "Start ‘anzu-query-replace’" anzu-isearch-query-replace
                                                        :if-nil buffer-read-only))
  (transient-replace-suffix 'casual-isearch-tmenu "x" '("x" "Start ‘anzu-query-replace-regexp’" anzu-isearch-query-replace-regexp
                                                        :if-nil buffer-read-only))
  ;; Add consult-line to Misc section
  (transient-append-suffix 'casual-isearch-tmenu "u" '("l" "consult-line" consult-line)))

(use-package isearch-dabbrev
  :after isearch
  :bind (:map isearch-mode-map ("M-/" . isearch-dabbrev-expand)))

(use-package anzu
  :diminish
  :config
  (global-anzu-mode)
  (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold)
  :custom
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-replace-threshold 100)
  (anzu-replace-to-string-separator " => ")
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp)
  (:map isearch-mode-map
        ([remap isearch-query-replace] . anzu-isearch-query-replace)
        ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

(use-package rg
  :bind
  ("C-c C-M-S-r" . rg-menu)
  ("C-c C-M-r" . rg)
  ("C-z" . rg-dwim)
  (:map search-map ("s" . rg)))

(use-package deadgrep
  :config
  (defun deadgrep-current-directory (search-term)
    (interactive (list (deadgrep--read-search-term)))
    (deadgrep search-term (file-name-directory buffer-file-name)))
  (defvar deadgrep--include-all nil)
  (defun deadgrep--include-all-advice (rg-args)
    (if deadgrep--include-all
        (push "-uuuLz" rg-args)
      rg-args))
  (advice-add 'deadgrep--arguments :filter-return #'deadgrep--include-all-advice)
  (defun deadgrep-all (search-term)
    (interactive (list (deadgrep--read-search-term)))
    (let ((deadgrep--include-all t))
      (deadgrep search-term)))
  :hook (deadgrep-mode . next-error-follow-minor-mode)
  :bind
  ("C-c c d" . deadgrep)
  ("C-c c M-d" . deadgrep-all)
  ("C-c c C-d" . deadgrep-current-directory)
  (:map deadgrep-mode-map
        ("e" . deadgrep-edit-mode)
        ("{" . deadgrep-backward-filename)
        ("}" . deadgrep-forward-filename))
  (:map search-map
        ("d" . deadgrep)
        ("M-d" . deadgrep-all)
        ("C-d" . deadgrep-current-directory)))

(use-package affe
  :config
  (setq affe-grep-command (replace-regexp-in-string "rg" "rg -Suu" affe-grep-command))
  ;; Configure Orderless
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  (defalias 'affe-grep-symbol-at-point 'affe-grep)
  (defalias 'affe-find-symbol-at-point 'affe-find)
  (consult-customize
   affe-grep :preview-key "M-."
   affe-grep-symbol-at-point :initial (thing-at-point 'symbol) :preview-key "M-."
   affe-find-symbol-at-point :initial (thing-at-point 'symbol) :preview-key "M-.")
  :bind
  ("C-c C-#" . affe-grep)
  ("C-c z" . affe-find)
  ("C-c Z" . affe-find-symbol-at-point)
  ("C-c C-~" . affe-grep-symbol-at-point)
  (:map search-map
        ("#" . affe-grep)
        ("~" . affe-grep-symbol-at-point)
        ("a" . affe-find)
        ("A" . affe-find-symbol-at-point)))

(provide 'init-search)
;;; init-search.el ends here
