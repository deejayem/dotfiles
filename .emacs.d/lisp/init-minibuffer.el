;;; init-minibuffer.el --- Minibuffer Completion Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Config for completion etc in the minibuffer (vertico, embark, consult, etc)
;; Most of it is taken from the READMEs and wikis of those packages.
;; Relies on orderless config in init-completion.el
;;; Code:

(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t))

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

  :bind (("C-M-<" . up-from-outside)
         ("C-M->" . down-from-outside)
         ("C-M-+" . preview-from-outside)
         ("M-X" . to-and-fro-minibuffer)
         ("C-M-S-g" . minibuffer-keyboard-quit)))

;; See init-packages.el for fetching of Vertico Extenions
;; Required extensions must be in the vertico-extensions var
(use-package vertico-directory
  :init
  (defvar switching-project nil)
  (defun vertico-directory-enter-or-switch-project ()
    "Wrapper around vertico-directory-enter that plays nicely with adding projects."
    (interactive)
    (if switching-project
        (vertico-exit)
      (vertico-directory-enter)))
  (defun read-project (orig &rest args)
    (let ((switching-project t))
      (apply orig args)))
  (advice-add 'project-prompt-project-dir :around
              'read-project)
  :config
  (defun vertico-directory-slash ()
    (interactive)
    (if (and (>= vertico--index 0)
             (string-suffix-p "/" (vertico--candidate))
             (vertico-directory--completing-file-p))
        (vertico-insert)
      (insert "/")))
  (defun vertico-directory-home ()
    (interactive)
    (if (and (string-suffix-p "/" (vertico--candidate))
             (vertico-directory--completing-file-p))
        (insert "~/")
      (insert "~")))
  :load-path vertico-extensions-dir
  :commands vertico-directory-enter
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter-or-switch-project)
              ("/" . vertico-directory-slash)
              ("~" . vertico-directory-home)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :load-path vertico-extensions-dir
  :bind ("M-P" . vertico-repeat))

(use-package consult
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
         ("M-*" . consult-line-symbol-at-point)
         ("C-c f" . consult-recent-file)
         ("C-c r" . consult-ripgrep)
         ("C-c R" . consult-ripgrep-auto-preview)
         ("C-c *" . consult-ripgrep-symbol-at-point)
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
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))                 ;; needed by consult-line to detect isearch

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
  (defun consult-z-ripgrep (&optional dir initial)
    (interactive "P")
    (let ((consult-ripgrep-args (replace-regexp-in-string "\\." "-z ." consult-ripgrep-args)))
      (consult-ripgrep dir initial)))
  (defun consult-buffer-no-preview ()
    (interactive)
    (consult-buffer))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended)))
      (when re
        (list :command (append
                        (list consult--fd-command
                              "--color=never" "--full-path"
                              (consult--join-regexps re 'extended))
                        opts)
              :highlight hl))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

  ;; Add these here, as we have two bindings for search map (M-s and C-c s)
  (define-key search-map "f" 'consult-fd)
  (define-key search-map "F" 'consult-find)
  (define-key search-map (kbd "M-f") 'consult-locate)
  (define-key search-map "g" 'consult-grep)
  (define-key search-map "G" 'consult-git-grep)
  (define-key search-map "r" 'consult-ripgrep)
  (define-key search-map "R" 'consult-ripgrep-auto-preview)
  (define-key search-map (kbd "M-r") 'consult-ripgrep-unrestricted)
  (define-key search-map "*" 'consult-ripgrep-symbol-at-point)
  (define-key search-map "z" 'consult-z-ripgrep)
  (define-key search-map "l" 'consult-line)
  (define-key search-map "L" 'consult-line-multi)
  (define-key search-map "m" 'consult-multi-occur)
  (define-key search-map "k" 'consult-keep-lines)
  (define-key search-map "u" 'consult-focus-lines)
  (define-key search-map "e" 'consult-isearch)

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

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (project-root project))))

  ;; Switch perspective when switching buffer if needed
  (setq consult--display-buffer #'persp-switch-to-buffer)

  (defvar consult-initial-narrow-config
    '((consult-buffer . ?x)
      (consult-buffer-no-preview . ?x)))
  ;; Add initial narrowing hook
  (defun consult-initial-narrow ()
    (when-let (key (alist-get this-command consult-initial-narrow-config))
      (setq unread-command-events (append unread-command-events (list key 32)))))
  (add-hook 'minibuffer-setup-hook #'consult-initial-narrow)

  (when (and (eq system-type 'darwin) (string-match-p "^find" consult-find-args))
    (setq consult-find-args (concat "g" consult-find-args)))

  (defvar consult--source-perspective-buffer
    `(:name     "Perspective Buffer"
                :narrow   (?x . "Perspective")
                :hidden   t
                :category buffer
                :face     consult-buffer
                :history  buffer-name-history
                :state    ,#'consult--buffer-state
                :enabled  ,(lambda () persp-mode)
                :items
                ,(lambda ()
                   (consult--buffer-query :sort 'visibility
                                          :predicate #'persp-is-current-buffer
                                          :as #'buffer-name)))
    "Perspective buffer candidate source for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-perspective-buffer t)

  ;; Copy of consult--source-project-file to use with perspective narrowing (identical except for narrowing key)
  ;; Put before consult--source-project-file so we get recentf behaviour here
  (defvar consult--source-perspective-files
    (plist-put (copy-sequence  consult--source-project-file) :narrow '(?x "Project Files")))
  (add-to-list 'consult-buffer-sources 'consult--source-perspective-files t)

  ;; Versions of consult--source-project-buffer and consult--source-project-file for use by consult-project-buffer
  ;; They allow narrowing with b and f (instead of p)
  ;; The file version uses fd to find items, so that all files (rather than using recentf) are listed, respecing .gitignore
  (defvar consult--project-source-project-buffer
    (plist-put (plist-put (copy-sequence consult--source-project-buffer)
                          :hidden nil)
               :narrow '(?b . "Project Buffer")))
  (defvar consult--project-source-project-file
    (plist-put (plist-put (plist-put (copy-sequence consult--source-project-file)
                                     :hidden nil)
                          :narrow '(?f . "Project File"))
               :items '(lambda ()
                         (when-let (root (consult--project-root))
                           (let ((len (length root))
                                 (inv-root (propertize root 'invisible t)))
                             (mapcar (lambda (x)
                                       (concat inv-root (substring x len)))
                                     (split-string
                                      (shell-command-to-string
                                       (format  "fd --color never -t f -0 . %s" root))
                                      "\0" t)))))))

  (defun consult-project-buffer ()
    (interactive)
    (let ((consult-buffer-sources '(consult--project-source-project-buffer
                                    consult--project-source-project-file)))
      (consult-buffer)))

  (defun consult--orderless-regexp-compiler (input type)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input str))))
  (defun consult--with-orderless (&rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
      (apply args)))
  (advice-add #'consult-ripgrep :around #'consult--with-orderless))

(use-package consult-flycheck)

(use-package consult-lsp
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (("C-," . embark-act)
   ("C-." . embark-dwim)
   ("M-." . embark-dwim)
   ("C-c C-o" . embark-export)
   ("C-h b" . embark-bindings)
   ("C-h B" . describe-bindings)
   (:map minibuffer-local-map
         ("M-." . embark-preview)))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; (define-key minibuffer-local-map (kbd "M-.") #'embark-preview)
  (defun embark-preview ()
    (interactive)
    (unless (bound-and-true-p consult--preview-function) ;; Disable preview for Consult commands
      (save-selected-window
        (let ((embark-quit-after-action))
          (embark-dwim)))))

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

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
