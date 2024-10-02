;;; init-minibuffer.el --- Minibuffer Completion Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Config for completion etc in the minibuffer (vertico, embark, consult, etc)
;; Most of it is taken from the READMEs and wikis of those packages.
;; Relies on orderless config in init-completion.el
;;; Code:

(use-package vertico
  :ensure (vertico :files (:defaults "extensions/*"))
  :hook (elpaca-after-init . vertico-mode)
  :custom (vertico-cycle t)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t intangible t face minibuffer-prompt))
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

  ;; https://github.com/minad/vertico/wiki#ding-when-wrapping-around
  (advice-add #'vertico-next
              :around
              #'(lambda (origin &rest args)
                  (let ((beg-index vertico--index))
                    (apply origin args)
                    (if (not (eq 1 (abs (- beg-index vertico--index))))
                        (ding)))))

  ;; https://github.com/minad/vertico/wiki#useful-commands-from-outside-minibuffer
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

  ;; Modified from https://github.com/minad/vertico/wiki#update-minibuffer-history-with-candidate-insertions
  (defadvice vertico-insert
      (after vertico-insert-add-history activate)
    "Make vertico-insert add to the minibuffer history."
    (if (and (not (eq minibuffer-history-variable t))
             (eq 'file (vertico--metadata-get 'category)))
        (add-to-history minibuffer-history-variable (minibuffer-contents))))

  ;; https://github.com/minad/vertico/wiki#customize-sorting-based-on-completion-category
  (defun sort-directories-first (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  (defun toggle-sort-directories-first ()
    (interactive)
    (if (eq vertico-sort-function 'sort-directories-first)
        (set (make-local-variable 'vertico-sort-function) 'vertico-sort-history-length-alpha)
      (set (make-local-variable 'vertico-sort-function) 'sort-directories-first))
    (setq vertico--input t)
    (vertico--update))

  :bind (("C-M-<" . up-from-outside)
         ("C-M->" . down-from-outside)
         ("C-M-+" . preview-from-outside)
         ("M-X" . to-and-fro-minibuffer)
         (:map vertico-map
               ("M-RET" . minibuffer-force-complete-and-exit)
               ("M-D" . toggle-sort-directories-first))))

(use-extension vertico vertico-multiform
  :config
  (vertico-multiform-mode +1)

  (defun vertico-multiform-buffer-grid ()
    "Toggle displaying Vertico as a grid in a large window (like a regular buffer).)"
    (interactive)
    (if (equal '(vertico-buffer-mode vertico-grid-mode) (car vertico-multiform--stack))
        (vertico-multiform-vertical)
      (setcar vertico-multiform--stack '(vertico-buffer-mode vertico-grid-mode))
      (vertico-multiform--toggle 1)))

  ;; https://github.com/minad/vertico/wiki#candidate-display-transformations-custom-candidate-highlighting
  (defvar +vertico-transform-functions nil)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))
  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir)
      file))
  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-doc-face."
    (let ((sym (intern cmd)))
      (if (or (eq sym major-mode)
              (and
               (memq sym minor-mode-list)
               (boundp sym)))
          (propertize cmd 'face 'font-lock-doc-face)
        cmd)))

  (setq vertico-multiform-commands
        '((execute-extended-command
           (+vertico-transform-functions . +vertico-highlight-enabled-mode))))
  (setq vertico-multiform-categories
        '((file (+vertico-transform-functions . +vertico-highlight-directory))
          (imenu grid)))
  :bind (:map vertico-multiform-map
              ("M-H" . vertico-multiform-buffer-grid)))

(use-extension vertico vertico-directory
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

  ;; TODO this should be part of the vertico config
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

(use-extension vertico vertico-repeat
  :after savehist
  :bind
  ("C-\\" . vertico-repeat)
  ("C-|" . vertico-repeat-select)
  :hook (minibuffer-setup . vertico-repeat-save)
  :config
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-extension vertico vertico-indexed
  :config
  (defmacro define-vertico-choose (n)
    `(defun ,(intern (format "vertico-indexed-choose-%s" n)) ()
       ,(format "Exit minibuffer with candidate %s." n)
       (interactive)
       (let ((vertico--index ,n))
         (funcall-interactively 'vertico-exit))))
  (defmacro define-vertico-insert (n)
    `(defun ,(intern (format "vertico-indexed-insert-%s" n)) ()
       ,(format "Insert candidate %s in minibuffer." n)
       (interactive)
       (let ((vertico--index ,n))
         (funcall-interactively 'vertico-insert))))
  (dotimes (n 10)
    (eval `(define-vertico-choose ,n))
    (eval `(define-vertico-insert ,n))
    (define-key vertico-map (kbd (format "C-%s" n)) (intern (format "vertico-indexed-choose-%s" n)))
    (define-key vertico-map (kbd (format "M-%s" n)) (intern (format "vertico-indexed-insert-%s" n))))
  (vertico-indexed-mode 1))

(use-extension vertico vertico-quick
  :bind (:map vertico-map
              ("M-;" . vertico-quick-insert)
              ("M-'" . vertico-quick-exit)))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c M-x" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x B" . consult-buffer-no-preview)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("C-," . consult-line)
         ("C-S-s" . consult-line)
         ("M-*" . consult-line-thing-at-point)
         ("C-#" . consult-line-thing-at-point)
         ("C-c f" . consult-recent-file)
         ("C-c r" . consult-ripgrep)
         ;; TODO find an alternative to C-c c?
         ("C-c c r" . consult-ripgrep-auto-preview)
         ("C-c c s" . consult-ripgrep-case-sensitive)
         ("C-c c z" . consult-z-ripgrep)
         ("C-c C-*" . consult-ripgrep-thing-at-point)
         ("C-c ." . consult-ripgrep-thing-at-point)
         ("C-c C-^" . consult-ripgrep-parent)
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         (:map isearch-mode-map
               ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
               ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
               ("M-s l" . consult-line)) ;; needed by consult-line to detect isearch
         (:map search-map
               ("f" . consult-fd)
               ("F" . consult-find)
               ("M-f" . consult-locate)
               ("g" . consult-grep)
               ("G" . consult-git-grep)
               ("r" . consult-ripgrep)
               ("R" . consult-ripgrep) ;; can't use r in isearch-mode, so add R too
               ("u" . consult-ripgrep-unrestricted)
               ("*" . consult-ripgrep-thing-at-point)
               ("z" . consult-z-ripgrep)
               ("^" . consult-ripgrep-parent)
               ("l" . consult-line)
               ("L" . consult-line-multi)
               ("m" . consult-multi-occur)
               ("k" . consult-keep-lines)
               ("C-f" . consult-focus-lines)
               ("e" . consult-isearch-history))
         (:map vertico-map
               ;; These are used for previewing with some consult commands (see consult-customize call below)
               ("C-S-p" . vertico-previous)
               ("C-S-n" . vertico-next)
               ;; Toggle preview on/off without changing preview-key
               ("M-P" . consult-toggle-preview)
               ("C-x C-M-x" . remove-leading-hash)))

  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

  ;; Configure register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (add-to-list 'consult-mode-histories '(cider-repl-mode cider-repl-input-history))

  (defvar consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  (consult-customize consult-line consult-line-thing-at-point :keymap consult-line-map)

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
  (defun consult-ripgrep-parent (&optional initial)
    (interactive "P")
    (consult-ripgrep (file-name-directory
                      (directory-file-name (or (persp-current-project-root) default-directory)))
                     initial))

  (defalias 'consult-line-thing-at-point 'consult-line)
  (defalias 'consult-ripgrep-thing-at-point 'consult-ripgrep)

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   ;; For these commands we can use C-N/C-P to scroll and preview, or M-. to preview
   consult-git-grep consult-grep
   consult-ripgrep-parent consult-ripgrep consult-ripgrep-case-sensitive
   consult-ripgrep-unrestricted consult-z-ripgrep consult-ripgrep-thing-at-point
   consult-bookmark consult-recent-file consult-xref consult-buffer-no-preview
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key '("M-." :debounce 0.2 "C-S-n" :debounce 0.2 "C-S-p")
   consult-ripgrep-thing-at-point
   :initial (concat "#" (thing-at-point 'symbol))
   consult-line-thing-at-point
   :initial (thing-at-point 'symbol))

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
  (append-to-list* 'consult-buffer-filter
                   "^\\*helpful"
                   "^\\*Warnings\\*"
                   "^\\*cider-test-report\\*"
                   "^\\*cider-error\\*"
                   "^\\*cider-inspect\\*")

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

  (defun remove-leading-hash ()
    "Remove a # character from the beginning of the current line.

Designed to be used for consult commands that automatically add a # at the beginning of the minibuffer.
See `+become' and the functions that call it (e.g. `+become-consult-line')."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (when (= ?# (char-after))
        (delete-forward-char 1))))

  (defun consult-project-buffer ()
    "Version of `consult-buffer' that only uses project-related sources."
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

(use-package consult-git-log-grep
  :bind ("C-c g l" . consult-git-log-grep)
  :custom (consult-git-log-grep-open-function #'magit-show-commit))

(use-package consult-ls-git
  :bind ("C-c g f" . consult-ls-git))

(use-package consult-project-extra)

(use-package marginalia
  :hook (elpaca-after-init . marginalia-mode)
  :config
  ;; crux-recentf-find-file
  (add-to-list 'marginalia-prompt-categories '("Choose recent file" . file)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-c C-o" . embark-export)
   ("C-h b" . embark-bindings)
   ("C-h B" . describe-bindings)
   (:map minibuffer-local-map
         ("M-." . embark-preview)
         ("C-," . embark-become)
         ("C-^" . embark-become-ripgrep-parent)
         ("C-S-SPC" . embark-select))
   (:map embark-become-file+buffer-map
         ("e" . consult-project-extra-find)
         ("E" . project-switch-consult-project-extra-find)))
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  :config
  (defalias 'embark-become-ripgrep-parent (kmacro "C-, ^"))
  (defun embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
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
  :config
  (defun +become (fn)
    "Remove the leading # from the minibuffer, and call `FN'.
Useful with embark-become, when changing from a command that uses # as a separator, to one that doesn't."
    (interactive)
    (setq unread-command-events (listify-key-sequence "\C-x\C-\M-x"))
    (funcall-interactively fn))
  (defun +become-consult-line ()
    "A version of `consult-line', designed for use with `embark-become'.
The leading # added by other consult commands is removed."
    (interactive)
    (+become #'consult-line))
  (defun +become-consult-line ()
    "A version of `consult-imenu', designed for use with `embark-become'.
The leading # added by other consult commands is removed."
    (interactive)
    (+become #'consult-imenu))
  :bind
  (:map embark-consult-async-search-map
        ("l" . +become-consult-line)
        ("f" . consult-focus-lines)
        ("i" . +become-consult-imenu)
        ("^" . consult-ripgrep-parent)
        ("u" . consult-ripgrep-unrestricted)
        ("c" . consult-ripgrep-case-sensitive)
        ("z" . consult-z-ripgrep))
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-todo
  ;; TODO use consult-todo-project when it works
  :bind ("C-c c t t" . consult-todo))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
