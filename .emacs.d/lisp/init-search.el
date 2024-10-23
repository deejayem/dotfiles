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

  ;; Modified from http://yummymelon.com/devnull/improving-emacs-isearch-usability-with-transient.html
  (transient-define-prefix isearch-menu ()
    "isearch Menu"
    [["Edit Search String"
      ("e"
       "Edit the search string (recursive)"
       isearch-edit-string
       :transient nil)
      ("w"
       "Pull next word or character word from buffer"
       isearch-yank-word-or-char
       :transient nil)
      ("s"
       "Pull next symbol or character from buffer"
       isearch-yank-symbol-or-char
       :transient nil)
      ("l"
       "Pull rest of line from buffer"
       isearch-yank-line
       :transient nil)
      ("y"
       "Pull string from kill ring"
       isearch-yank-kill
       :transient nil)
      ("t"
       "Pull thing from buffer"
       isearch-forward-thing-at-point
       :transient nil)]

     ["Replace"
      ("q"
       "Start ‘query-replace’"
       anzu-isearch-query-replace
       :if-nil buffer-read-only
       :transient nil)
      ("x"
       "Start ‘query-replace-regexp’"
       anzu-isearch-query-replace-regexp
       :if-nil buffer-read-only
       :transient nil)]]

    [["Toggle"
      ("X"
       "Toggle regexp searching"
       isearch-toggle-regexp
       :transient nil)
      ("S"
       "Toggle symbol searching"
       isearch-toggle-symbol
       :transient nil)
      ("W"
       "Toggle word searching"
       isearch-toggle-word
       :transient nil)
      ("F"
       "Toggle case fold"
       isearch-toggle-case-fold
       :transient nil)
      ("L"
       "Toggle lax whitespace"
       isearch-toggle-lax-whitespace
       :transient nil)]

     ["Misc"
      ("o"
       "occur"
       isearch-occur
       :transient nil)
      ("h"
       "highlight"
       isearch-highlight-regexp
       :transient nil)
      ("H"
       "highlight lines"
       isearch-highlight-lines-matching-regexp
       :transient nil)
      ("l"
       "consult-line"
       consult-line
       :transient nil)
      ("<"
       "isearch-beginning-of-buffer"
       isearch-beginning-of-buffer
       :transient nil)
      (">"
       "isearch-end-of-buffer"
       isearch-end-of-buffer
       :transient nil)]])

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
  ("C-*" . isearch-forward-symbol-at-point)
  (:map isearch-mode-map ("<f2>" . isearch-menu))
  (:map search-map
        ("<" . isearch-beginning-of-buffer)
        (">" . isearch-end-of-buffer)))

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
  (defun deadgrep-symbol-at-point ()
    (interactive)
    (deadgrep (thing-at-point 'symbol)))
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
  ("C-S-z" . deadgrep-symbol-at-point)
  ("C-c c C-d" . deadgrep-current-directory)
  (:map deadgrep-mode-map
        ("e" . deadgrep-edit-mode)
        ("{" . deadgrep-backward-filename)
        ("}" . deadgrep-forward-filename))
  (:map search-map
        ("d" . deadgrep)
        ("M-d" . deadgrep-all)
        ("C-d" . deadgrep-current-directory)
        ("D" . deadgrep-symbol-at-point)))

(use-package affe
  :config
  (setq affe-grep-command (replace-regexp-in-string "rg" "rg -Suu" affe-grep-command))
  ;; Configure Orderless
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
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
  (consult-customize affe-grep my/affe-grep-symbol-at-point :preview-key "M-.")
  :bind
  ("C-c C-#" . affe-grep)
  ("C-c z" . affe-find)
  ("C-c Z" . my/affe-find-symbol-at-point)
  ("C-~" . my/affe-grep-symbol-at-point)
  (:map search-map
        ("#" . affe-grep)
        ("~" . my/affe-grep-symbol-at-point)
        ("a" . affe-find)
        ("A" . my/affe-find-symbol-at-point)))

(provide 'init-search)
;;; init-search.el ends here
