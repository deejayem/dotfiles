;;; init-completion.el --- Completion Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Config for completion-at-point (corfu), as well as orderless (see also init-minibuffer.el)
;; Most of it is taken from the READMEs and wikis of those packages
;;; Code:

(use-feature dabbrev
  ;; Load newer version until 30.1 is released
  ;; https://mail.gnu.org/archive/html/bug-gnu-emacs/2024-05/msg00422.html
  :load-path "elpaca/repos/project/lisp/dabbrev.el"
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
    (if (and (or smartparens-mode paredit-mode) (equal (substring str -1) ")"))
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

(use-feature emacs
  :config
  (setq completion-cycle-threshold 2)
  (setq tab-always-indent 'complete)
  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t))

(use-package orderless
  :bind
  (:map minibuffer-local-map
        ("C-l" . orderless-toggle-literal-matching))
  (:map corfu-map
        ("C-l" . orderless-toggle-literal-matching))
  :custom
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion orderless))
                                   (command (styles +orderless-with-strict-leading-initialism))
                                   (variable (styles +orderless-with-strict-leading-initialism))
                                   (symbol (styles +orderless-with-strict-leading-initialism))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-strict-initialism))
  (orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                     #'orderless-affix-dispatch))
  :config
  ;; Inspired by https://github.com/oantolin/orderless/blob/ac4aeb66f331f4c4a430d5556071e33177304c37/README.org#interactively-changing-the-configuration
  (defun orderless-toggle-literal-matching ()
    "Toggle matching components literally for the rest of the session."
    (interactive)
    (if orderless-style-dispatchers
        (progn
          (setq-local +saved-orderless-matching-styles orderless-matching-styles
                      +saved-orderless-style-dispatchers orderless-style-dispatchers)
          (setq-local orderless-matching-styles '(orderless-literal)
                      orderless-style-dispatchers nil))
      (setq-local orderless-matching-styles +saved-orderless-matching-styles
                  orderless-style-dispatchers +saved-orderless-style-dispatchers))
    (when vertico--input
      (setq vertico--input t)
      (vertico--update))
    (when corfu--input
      (setq corfu--input t)
      (corfu--update)))

  (defun orderless-strict-initialism (component &optional leading)
    "Match a component as a strict initialism.
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

  (setf (alist-get ?, orderless-affix-dispatch-alist) #'orderless-strict-leading-initialism)
  (add-to-list 'orderless-affix-dispatch-alist '(?` . orderless-strict-initialism) t)

  ;; Copied from https://github.com/minad/consult/wiki#minads-orderless-configuration
  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Copied from https://github.com/minad/consult/wiki#minads-orderless-configuration
  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;; Based on https://github.com/minad/consult/wiki#minads-orderless-configuration
  (orderless-define-completion-style +orderless-with-strict-leading-initialism
    (orderless-matching-styles '(orderless-literal orderless-regexp orderless-strict-leading-initialism))))

;; code completion - corfu
(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*"))
  :custom
  (corfu-cycle t)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :hook (elpaca-after-init . global-corfu-mode))

(use-extension corfu corfu-indexed
  :config
  (defmacro define-corfu-complete (n)
    `(defun ,(intern (format "corfu-indexed-complete-%s" n)) ()
       ,(format "Complete with candidate %s." n)
       (interactive)
       (let ((corfu--index ,n))
         (funcall-interactively 'corfu-complete))))
  (defmacro define-corfu-insert (n)
    `(defun ,(intern (format "corfu-indexed-insert-%s" n)) ()
       ,(format "Insert candidate %s." n)
       (interactive)
       (let ((corfu--index ,n))
         (funcall-interactively 'corfu-insert))))
  (dotimes (n 10)
    (eval `(define-corfu-complete ,n))
    (eval `(define-corfu-insert ,n))
    (define-key corfu-map (kbd (format "C-%s" n)) (intern (format "corfu-indexed-complete-%s" n)))
    (define-key corfu-map (kbd (format "M-%s" n)) (intern (format "corfu-indexed-insert-%s" n))))
  (corfu-indexed-mode 1))

(use-extension corfu corfu-quick
  :bind (:map corfu-map
              ("M-;" . corfu-quick-insert)
              ("M-'" . corfu-quick-complete)))

(use-extension corfu corfu-history
  :after savehist
  :config
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-extension corfu corfu-popupinfo
  :hook (global-corfu-mode . corfu-popupinfo-mode))

(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
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
  (add-to-list 'completion-at-point-functions #'cape-dict t))

(provide 'init-completion)
;;; init-completion.el ends here
