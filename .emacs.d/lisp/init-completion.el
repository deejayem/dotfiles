;;; init-completion.el --- Completion Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Config for completion-at-point (corfu), as well as orderless (see also init-minibuffer.el)
;; Most of it is taken from the READMEs and wikis of those packages
;;; Code:

(use-package dabbrev
  :diminish
  :custom
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil))

(use-package hippie-expand
  :ensure nil
  :init
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
  :config
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
  :bind (:map minibuffer-local-map
              ("C-l" . my/orderless-match-components-literally))
  :custom (orderless-component-separator 'orderless-escapable-split-on-space)
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion orderless)))))

  (defun my/orderless-match-components-literally ()
    "Components match literally for the rest of the session."
    (interactive)
    (setq-local orderless-matching-styles '(orderless-literal)
                orderless-style-dispatchers nil))
  :config
  (defun orderless-strict-leading-initialism (component)
    "Match a component as a strict leading initialism.
This means the characters in COMPONENT must occur in the
candidate, in that order, at the beginning of words, with
no words in between, beginning with the first word."
    (orderless--separated-by '(seq (zero-or-more word) (zero-or-more punct))
      (cl-loop for char across component collect `(seq word-start ,char))
      '(seq string-start)))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun my/orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((string-match-p "\\`\\.." pattern) `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-strict-leading-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-strict-leading-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (setq orderless-matching-styles '(orderless-literal orderless-prefixes orderless-regexp orderless-strict-leading-initialism)
        orderless-style-dispatchers '(my/orderless-dispatch)))

;; code completion - corfu
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)            ;; Enable cycling for `corfu-next/previous'
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :config
  (add-hook 'corfu-mode-hook #'corfu-doc-mode))

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
