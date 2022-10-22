;;; init-completion.el --- Completion Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Config for completion-at-point (corfu), as well as orderless (see also init-minibuffer.el)
;; Most of it is taken from the READMEs and wikis of those packages
;;; Code:

(use-feature dabbrev
  :diminish
  :custom
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil))

(use-feature hippie-expand
  :config
  (setq hippie-expand-try-functions-list
      '(;yas-hippie-try-expand
        try-expand-dabbrev
        try-expand-all-abbrevs
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
  ;; https://www.emacswiki.org/emacs/HippieExpand#h5o-9
  (defadvice he-substitute-string (after he-paredit-fix)
    "Remove extra paren when expanding line in paredit."
    (if (and paredit-mode (equal (substring str -1) ")"))
        (progn (backward-delete-char 1) (forward-char))))
  :bind
  ("C-M-/" . hippie-expand))

(use-package fancy-dabbrev
  :diminish
  :config
  (global-fancy-dabbrev-mode)
  (defun fancy-dabbrev-popup-advice (_next)
    (local-set-key (kbd "C-M-/") #'fancy-dabbrev-backward))
  (defun fancy-dabbrev-popup-exit-advice ()
    (local-unset-key (kbd "C-M-/")))
  (advice-add #'fancy-dabbrev--expand-again :before #'fancy-dabbrev-popup-advice)
  (advice-add #'fancy-dabbrev--on-exit :after #'fancy-dabbrev-popup-exit-advice)
  :bind ("M-/" . fancy-dabbrev-expand))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package orderless
  :defer 2
  :bind (:map minibuffer-local-map
              ("C-l" . my/orderless-match-components-literally))
  :custom
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion orderless)))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-strict-leading-initialism))
  (orderless-style-dispatchers '(+orderless-dispatch))
  :config
  (defun my/orderless-match-components-literally ()
    "Components match literally for the rest of the session."
    (interactive)
    (setq-local orderless-matching-styles '(orderless-literal)
                orderless-style-dispatchers nil))

  (defun orderless-strict-initialism (component &optional leading)
    "Match a component as a strict leading initialism.
This means the characters in COMPONENT must occur in the
candidate, in that order, at the beginning of words, with
no words in between. If LEADING is non-nil, anchor to the
first word."
    (orderless--separated-by '(seq (zero-or-more word) (zero-or-more punct))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when leading '(seq buffer-start))))

  (defun orderless-strict-leading-initialism (component)
    "Match a component as a strict leading initialism.
This means the characters in COMPONENT must occur in the
candidate, in that order, at the beginning of words, with
no words in between, beginning with the first word."
    (orderless-strict-initialism component t))

  ;; based on https://github.com/minad/consult/wiki#minads-orderless-configuration
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?` . orderless-strict-initialism)
      (?= . orderless-literal)
      (?_ . orderless-prefix)
      (?~ . orderless-flex)))

  (defun +orderless--suffix-regexp ()
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * _prefix prefix_
  ;; * %char-fold char-fold%
  ;; * `strict-initialism strict-initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--suffix-regexp))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--suffix-regexp))))
     ;; Ignore single !
     ((equal "!" word) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref word 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 1))
        (when-let (x (assq (aref word (1- (length word))) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 0 -1))))))))

;; code completion - corfu
(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-indexed corfu-quick corfu-history corfu-info))
  :custom
  (corfu-cycle t)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :hook (emacs-startup . global-corfu-mode))

(use-extension corfu corfu-indexed
  :config (corfu-indexed-mode 1))

(use-extension corfu corfu-quick
  :bind (:map corfu-map
              ("M-;" . corfu-quick-insert)
              ("M-'" . corfu-quick-exit)))

(use-extension corfu corfu-history
  :config
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-doc
  :hook
  (corfu-mode . corfu-doc-mode))

(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :custom
  (cape-dict-file "/usr/share/dict/words")
  :init
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-dict t)
  (add-to-list 'completion-at-point-functions #'cape-ispell t))

(provide 'init-completion)
;;; init-completion.el ends here
