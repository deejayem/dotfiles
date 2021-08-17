;;; init-completion.el --- Commpletion Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Config for completion-at-point (corfu) and minibuffer (vertico, embark, consult, etc)
;;; Code:

(use-package dabbrev
  :diminish)

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
  :bind
  ("M-/" . hippie-expand))

(use-package emacs
  :init
  ;; for corfu
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)

  ;; for vertico
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

;; orderless is used by corfu and vertico
(use-package orderless
  :bind (:map minibuffer-local-completion-map
              ("C-l" . my/orderless-match-components-literally))
  :custom (orderless-component-separator 'orderless-escapable-split-on-space)
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion orderless)))))

  (defun my/orderless-match-components-literally ()
    "Components match literally for the rest of the session."
    (interactive)
    (setq-local orderless-matching-styles '(orderless-literal)
                orderless-style-dispatchers nil))
  :config
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
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-strict-leading-initialism)
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
  (corfu-global-mode))

(use-package fancy-dabbrev
  :diminish
  :config
  (global-fancy-dabbrev-mode))

;; minibuffer completion - vertico et al
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t)
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))
  :config
  (defun down-from-outside ()
    "Move to next candidate in minibuffer, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro [down])))

  (defun up-from-outside ()
    "Move to previous candidate in minibuffer, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro [up])))

  (defun preview-from-outside ()
    "Preview the selected candidate, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro (kbd "M-."))))

  (defun to-and-fro-minibuffer ()
    "Go back and forth between minibuffer and other window."
    (interactive)
    (if (window-minibuffer-p (selected-window))
        (select-window (minibuffer-selected-window))
      (select-window (active-minibuffer-window))))

  (key-chord-define-global "XX" 'to-and-fro-minibuffer)
  ;(key-chord-define-global ">>" 'preview-from-outside)
  :bind (("C-M-<" . up-from-outside)
         ("C-M->" . down-from-outside)
         ("C-M-+" . preview-from-outside)
         ("M-X" . to-and-fro-minibuffer)
         ("C-M-S-g" . minibuffer-keyboard-quit)))

;; See init-packages.el for fetching of Vertico Extenions
;; Required extensions must be in the vertico-extensions var
(use-package vertico-directory
  :load-path "site-lisp/vertico-extensions/vertico-directory.el"
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("/" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :load-path "site-lisp/vertico-extensions/vertico-repeat.el"
  :bind ("M-P" . vertico-repeat))

(use-package consult
  :after projectile
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x B" . consult-buffer-no-preview)     ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("C-S-s" . consult-line)
         ("C-M-S" . consult-line-symbol-at-point)
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; (Mostly) C-c c bindings (search-map)
         ("C-c f" . consult-recent-file)
         ("C-c c f" . consult-find)
         ("C-c c L" . consult-locate)
         ("C-c c g" . consult-grep)
         ("C-c c G" . consult-git-grep)
         ("C-c c r" . consult-ripgrep)
         ("C-c r" . consult-ripgrep)
         ("C-c c R" . consult-ripgrep-auto-preview)
         ("C-c R" . consult-ripgrep-auto-preview)
         ("C-c c M-r" . consult-ripgrep-unrestricted)
         ("C-c c s" . consult-ripgrep-symbol-at-point)
         ("C-c c l" . consult-line)
         ("C-c c m" . consult-multi-occur)
         ("C-c c k" . consult-keep-lines)
         ("C-c c u" . consult-focus-lines)
         ;; Isearch integration
         ("C-c c e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("C-c c e" . consult-isearch)             ;; orig. isearch-edit-string
         ("C-c c l" . consult-line))               ;; needed by consult-line to detect isearch

  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-ripgrep-unrestricted consult-ripgrep-symbol-at-point
   consult-bookmark consult-recent-file consult-xref consult-buffer-no-preview
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  (defvar consult-initial-narrow-config
    '((consult-buffer . ?p)
      (consult-buffer-no-preview . ?p)))
  ;; Add initial narrowing hook
  (defun consult-initial-narrow ()
    (when-let (key (alist-get this-command consult-initial-narrow-config))
      (setq unread-command-events (append unread-command-events (list key 32)))))
  (add-hook 'minibuffer-setup-hook #'consult-initial-narrow)

  (when (and (eq system-type 'darwin) (string-match-p "^find" consult-find-command))
    (setq consult-find-command (concat "g" consult-find-command)))

  (defun consult--orderless-regexp-compiler (input type)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input str))))
  ;; (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)
  (defun consult--with-orderless (&rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
      (apply args)))
  (advice-add #'consult-ripgrep :around #'consult--with-orderless)

  (defun consult-ripgrep-symbol-at-point (&optional dir initial)
    (interactive
      (list prefix-arg (when-let ((s (symbol-at-point)))
                         (symbol-name s))))
    (consult-ripgrep dir initial))
  (defun consult-ripgrep-auto-preview (&optional dir initial)
    (interactive "P")
    (consult-ripgrep dir initial))
  (defun consult-ripgrep-unrestricted (&optional dir initial)
    (interactive "P")
    (let ((consult-ripgrep-args (replace-regexp-in-string "\\." "-uu ." consult-ripgrep-args)))
      (consult-ripgrep dir initial)))
  (defun consult-buffer-no-preview ()
    (interactive)
    (consult-buffer))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol))))

(use-package consult-flycheck)

(use-package consult-lsp
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map ;minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  ;; For Projectile
  (add-to-list 'marginalia-prompt-categories '("Switch to project" . file))
  (add-to-list 'marginalia-prompt-categories '("Find file" . project-file))
  (add-to-list 'marginalia-prompt-categories '("Recently visited files" . project-file))
  (add-to-list 'marginalia-prompt-categories '("Switch to buffer" . buffer)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-c C-o" . embark-export)
   ("C-h b" . embark-bindings)
   ("C-h B" . describe-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-completion)
;;; init-completion.el ends here
