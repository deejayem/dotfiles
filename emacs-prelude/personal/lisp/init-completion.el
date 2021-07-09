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
(prelude-require-package 'orderless)
(use-package orderless
  :bind (:map minibuffer-local-completion-map
              ("C-l" . my/match-components-literally))
  :custom (orderless-component-separator 'orderless-escapable-split-on-space)
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion orderless)))))

  (defun my/match-components-literally ()
    "Components match literally for the rest of the session."
    (interactive)
    (setq-local orderless-matching-styles '(orderless-literal)
                orderless-style-dispatchers nil))
  :config
  (defun flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun literal-if-hat (pattern _index _total)
    (when (string-suffix-p "^" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun literal-if-apostrophe (pattern _index _total)
    (cond
     ((equal "'" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "'" pattern)
      `(orderless-literal . ,(substring pattern 1)))))

  (defun initialism-if-comma (pattern _index _total)
    (cond
     ((equal "," pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "," pattern)
      `(orderless-initialism . ,(substring pattern 1)))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-strict-leading-initialism)
        orderless-style-dispatchers '(flex-if-twiddle
                                      literal-if-hat
                                      literal-if-apostrophe
                                      initialism-if-comma
                                      without-if-bang)))

(use-package savehist
  :init
  (savehist-mode))


;; code completion - corfu
(prelude-require-package 'corfu)
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

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))


;; minibuffer completion - vertico et al
(prelude-require-package 'vertico)
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(prelude-require-package 'consult)
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
         ("s-s" . consult-line-symbol-at-point)
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
         ;; C-c c bindings (search-map)
         ("C-c c C-f" . consult-recent-file)
         ("C-c c f" . consult-fd)
         ("C-c c F" . consult-find)
         ("C-c c L" . consult-locate)
         ("C-c c g" . consult-grep)
         ("C-c c G" . consult-git-grep)
         ("C-c c r" . consult-smart-ripgrep)
         ("C-c c R" . consult-ripgrep-auto-preview)
         ("C-c c C-M-r" . consult-iripgrep)
         ("C-c c M-r" . consult-ripgrep-unrestricted)
         ("C-c c C-r" . consult-ripgrep)
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
   consult-smart-ripgrep consult-iripgrep consult-ripgrep-unrestricted
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

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial)))
  (defun consult-smart-ripgrep (&optional dir initial)
    (interactive "P")
    (let ((consult-ripgrep-command "rg -S --null --line-buffered --color=ansi --max-columns=1000   --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep dir initial)))
  (defun consult-ripgrep-symbol-at-point (&optional dir initial)
    (interactive
      (list prefix-arg (when-let ((s (symbol-at-point)))
                         (symbol-name s))))
    (consult-smart-ripgrep dir initial))
  (defun consult-ripgrep-auto-preview (&optional dir initial)
    (interactive "P")
    (consult-smart-ripgrep dir initial))
  (defun consult-iripgrep (&optional dir initial)
    (interactive "P")
    (let ((consult-ripgrep-command "rg -i --null --line-buffered --color=ansi --max-columns=1000   --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep dir initial)))
  (defun consult-ripgrep-unrestricted (&optional dir initial)
    (interactive "P")
    (let ((consult-ripgrep-command "rg -S -uu --null --line-buffered --color=ansi --max-columns=1000   --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep dir initial)))
  (defun consult-buffer-no-preview ()
    (interactive)
    (consult-buffer))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol))))

(prelude-require-package 'consult-flycheck)
(use-package consult-flycheck)

(prelude-require-package 'consult-lsp)
(use-package consult-lsp
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . consult-lsp-symbols)))

(prelude-require-package 'marginalia)
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;:custom
  ;(marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(prelude-require-package 'embark)
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-c C-o" . embark-export)
   ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(prelude-require-package 'embark-consult)
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; async fuzzy finder (uses consult and orderless)
(prelude-require-package 'affe)
(use-package affe
  :after orderless
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))
  (defun my/affe-grep-symbol-at-point (&optional dir initial)
    (interactive
      (list prefix-arg (when-let ((s (symbol-at-point)))
                         (symbol-name s))))
    (affe-grep dir initial))
  :bind
  (("C-c c a g" . affe-grep)
   ("C-c c a f" . affe-find)
   ("C-c c a s" . my/affe-grep-symbol-at-point)))

(provide 'init-completion)
