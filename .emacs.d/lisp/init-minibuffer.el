;;; init-minibuffer.el --- Minibuffer Completion Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Config for completion etc in the minibuffer (vertico, embark, consult, etc)
;; Most of it is taken from the READMEs and wikis of those packages.
;; Relies on orderless config in init-completion.el
;;; Code:

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-directory vertico-repeat vertico-indexed vertico-quick))
  :hook (emacs-startup . vertico-mode)
  :custom (vertico-cycle t)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index start)
                (setq cand (funcall orig cand prefix suffix index start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))
  (defun define-vertico-key (key &rest defs)
    "Define KEY conditionally in the vertico keymap.
DEFS is a plist associating completion categories to commands."
    (let ((default-command (lookup-key vertico-map (kbd key))))
      (define-key vertico-map (kbd key)
        (list 'menu-item nil defs :filter
              (lambda (d)
                (or (plist-get d (completion-metadata-get
                                  (completion-metadata (minibuffer-contents)
                                                       minibuffer-completion-table
                                                       minibuffer-completion-predicate)
                                  'category))
                    default-command))))))

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

  (defun minibuffer-really-quit ()
    "Quit minibuffer session, even if it is not the selected window."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (minibuffer-keyboard-quit)))

  :bind (("C-M-<" . up-from-outside)
         ("C-M->" . down-from-outside)
         ("C-M-+" . preview-from-outside)
         ("M-X" . to-and-fro-minibuffer)
         ("C-M-S-g" . minibuffer-really-quit)
         (:map vertico-map ("M-RET" . minibuffer-force-complete-and-exit))))

(use-feature vertico-directory
  :after vertico
  :config
  (defvar switching-project nil)
  (defun vertico-directory-enter-or-select-project ()
    "vertico-directory-enter wrapper that plays nicely with selecting new projects."
    (interactive)
    ;; When selecting a project, use this to return, instead of entering the directory
    (if switching-project
        (vertico-exit)
      (vertico-directory-enter)))
  (defun vertico-directory-slash ()
    (interactive)
    (if (and (>= vertico--index 0)
             (string-suffix-p "/" (vertico--candidate))
             (eq 'file (vertico--metadata-get 'category)))
        (vertico-insert)
      (insert "/")))
  (defun vertico-directory-home ()
    (interactive)
    (if (and (string-suffix-p "/" (vertico--candidate))
             (eq 'file (vertico--metadata-get 'category)))
        (insert "~/")
      (insert "~")))
  (defun read-project (orig &rest args)
    (let ((switching-project t))
      (apply orig args)))
  (advice-add 'project-prompt-project-dir :around
              'read-project)
  (define-vertico-key "/"
    'file #'vertico-directory-slash
    'project-file #'vertico-directory-slash)
  (define-vertico-key "RET"
    'file #'vertico-directory-enter-or-select-project
    'project-file #'vertico-directory-enter)
  (define-vertico-key "~"
    'file #'vertico-directory-home)
  (define-vertico-key "DEL"
    'file #'vertico-directory-delete-char
    'project-file #'vertico-directory-delete-char)
  (define-vertico-key "M-DEL"
    'file #'vertico-directory-delete-word
    'project-file #'vertico-directory-delete-word)
  :commands (vertico-directory-enter vertico-directory-delete-word vertico-directory-delete-char)
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-feature vertico-repeat
  :bind ("<f9>" . vertico-repeat)
  :hook (minibuffer-setup . vertico-repeat-save)
  :config
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-feature vertico-indexed
  :after vertico
  :demand t
  :config (vertico-indexed-mode 1))

(use-feature vertico-quick
  :after vertico
  :demand t
  :bind (:map vertico-map
              ("C-;" . vertico-quick-insert)
              ("C-'" . vertico-quick-exit)))

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
         ;; TODO find an alternative to C-c c?
         ("C-c c r" . consult-ripgrep-auto-preview)
         ("C-c c s" . consult-ripgrep-case-sensitive)
         ("C-c c z" . consult-z-ripgrep)
         ("C-c C-*" . consult-ripgrep-symbol-at-point)
         ("C-c C-^" . consult-ripgrep-parent)
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
         ("M-g I" . consult-imenu-multi)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         (:map vertico-map
               ;; These are used for previewing with some consult commands (see consult-customize call below)
               ("C-S-p" . vertico-previous)
               ("C-S-n" . vertico-next)
               ;; Toggle preview on/off without changing preview-key
               ("M-P" . consult-toggle-preview)))

  :config

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

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
  (defun consult-ripgrep-case-sensitive (&optional dir initial)
    (interactive "P")
    (let ((consult-ripgrep-args (replace-regexp-in-string "\\." "-s ." consult-ripgrep-args)))
      (consult-ripgrep dir initial)))
  (defun consult-buffer-no-preview ()
    (interactive)
    (consult-buffer))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defun consult-ripgrep-parent (&optional initial)
    (interactive "P")
    (consult-ripgrep (file-name-directory (directory-file-name (persp-current-project-root))) initial))
  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
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

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   ;; For these commands we can use C-S/C-P to scoll and preview, or M-. to preview
   consult-ripgrep consult-git-grep consult-grep
   consult-ripgrep-unrestricted consult-ripgrep-symbol-at-point
   consult-bookmark consult-recent-file consult-xref consult-buffer-no-preview
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (list (kbd "M-.") (kbd "C-S-n") (kbd "C-S-p")))

  (defvar-local consult-toggle-preview-orig nil)
  (defun consult-toggle-preview ()
    "Command to enable/disable preview."
    (interactive)
    (if consult-toggle-preview-orig
        (setq consult--preview-function consult-toggle-preview-orig
              consult-toggle-preview-orig nil)
      (setq consult-toggle-preview-orig consult--preview-function
            consult--preview-function #'ignore)))

  (setq consult-narrow-key "<")

  (setq consult-project-function (lambda (_) (persp-current-project-root)))

  ;; Switches perspective if we select a buffer from another perspective, but note that previewing
  ;; a buffer adds it to the current perspective, so preview should be disabled before removing
  ;; perspective narrowing
  (defun consult--persp-buffer-action (orig &rest args)
    (when (not (cdr args)) ;; (cdr args) is norecord, which should distinguish preview/non-preview
      (let ((buffer (window-normalize-buffer-to-switch-to (car args))))
        (unless (persp-is-current-buffer buffer)
          (let ((other-persp (persp-buffer-in-other-p buffer)))
            (when (eq (car-safe other-persp) (selected-frame))
              (persp-switch (cdr other-persp)))))))
    (apply orig args))
  (advice-add 'consult--buffer-action :around 'consult--persp-buffer-action)

  (defvar consult-initial-narrow-config
    '((consult-buffer . ?x)
      (consult-buffer-no-preview . ?x)
      (consult-buffer-other-window . ?x)
      (consult-project-extra-find . ?f)))
  ;; Add initial narrowing hook
  (defun consult-initial-narrow ()
    (when-let (key (alist-get this-command consult-initial-narrow-config))
      (setq unread-command-events (append unread-command-events (list key 32)))))
  (add-hook 'minibuffer-setup-hook #'consult-initial-narrow)

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
    (plist-put (plist-put (copy-sequence  consult--source-project-recent-file)
                          :name "Project File")
               :narrow '(?x . "Perspective")))
  (add-to-list 'consult-buffer-sources 'consult--source-perspective-files t)

  ;; Versions of consult--source-project-buffer and consult--source-project-file for use by consult-project-buffer
  ;; They allow narrowing with b, f and a (instead of p)
  ;; f is the recentf version provided by consult
  ;; a is an "all files" version based on fd (respecting .gitignore, hidden by default)
  (defvar consult--project-source-project-buffer
    (plist-put (plist-put (copy-sequence consult--source-project-buffer)
                          :hidden nil)
               :narrow '(?b . "Buffer")))
  (defvar consult--project-source-project-file-recentf
    (plist-put (plist-put (copy-sequence consult--source-project-recent-file)
                          :hidden nil)
               :narrow '(?f . "File (Recentf)")))
  (defvar consult--project-source-project-file-all
    (plist-put (plist-put (copy-sequence consult--source-project-recent-file)
                          :narrow '(?a . "File (All)"))
               :items '(lambda ()
                         (when (eq 0 (call-process-shell-command "fd"))
                           (when-let (root (consult--project-root))
                             (let ((len (length root))
                                   (inv-root (propertize root 'invisible t)))
                               (mapcar (lambda (x)
                                         (concat inv-root (substring x len)))
                                       (split-string
                                        (shell-command-to-string
                                         (format  "fd --color never -t f -0 . %s" root))
                                        "\0" t))))))))

  (defun consult-project-buffer ()
    (interactive)
    (let ((consult-buffer-sources '(consult--project-source-project-buffer
                                    consult--project-source-project-file-recentf
                                    consult--project-source-project-file-all)))
      (consult-buffer))))

(use-package consult-flycheck)

(use-package consult-lsp
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-ls-git)
(use-package consult-project-extra)

(use-package marginalia
  :hook (emacs-startup . marginalia-mode)
  :config
  ;; crux-recentf-find-file
  (add-to-list 'marginalia-prompt-categories '("Choose recent file" . file)))

(use-package embark
  :bind
  (("C-," . embark-act)
   ("C-." . embark-dwim)
   ("M-." . embark-dwim)
   ("C-c C-o" . embark-export)
   ("C-h b" . embark-bindings)
   ("C-h B" . describe-bindings)
   (:map minibuffer-local-map
         ("M-." . embark-preview))
   (:map embark-become-file+buffer-map
         ("e" . consult-project-extra-find)
         ("E" . project-switch-consult-project-extra-find)))
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  :config
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
  ;; demand, combined with after means that this will load after embark and consult
  ;; See https://github.com/oantolin/embark/commit/47daded610b245caf01a97d74c940aff91fe14e2#r46010972
  :demand t
  :bind
  (:map embark-consult-async-search-map
        ("^" . consult-ripgrep-parent)
        ("R" . consult-ripgrep-unrestricted))
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
