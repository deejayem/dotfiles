;;; init-ui.el --- UI Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Some parts copied from prelude-ui.el and prelude-editor.el
;; Frame customisations, and disabling of {menu,tool,scoll}-bar-mode done in early-init.el, rather than here
;;; Code:

(use-feature emacs
  :hook
  (elpaca-after-init . (lambda ()
                         (cond
                          ((find-font (font-spec :name "iosevka comfy"))
                           (set-face-attribute 'default nil :font "iosevka comfy"))
                          ((find-font (font-spec :name "iosevka"))
                           (set-face-attribute 'default nil :font "iosevka")))

                         (global-display-line-numbers-mode)
                         (global-hl-line-mode +1)

                         (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
                         (load-theme 'non-modo t)))
  :bind ("C-x C-S-k" . kill-buffer)
  :config
  (setq-default display-line-numbers-widen t)
  ;; https://github.com/rougier/elegant-emacs/blob/master/sanity.el
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        inhibit-startup-message t
        initial-scratch-message nil)

  (blink-cursor-mode -1)
  (setq-default cursor-type 'bar)

  (setq show-trailing-whitespace t)
  (setq-default indicate-empty-lines t)

  ;; http://whattheemacsd.com/appearance.el-02.html (if this gets to annoying, just set back to 'ignore)
  (setq ring-bell-function (lambda ()
                             (invert-face 'mode-line)
                             (run-with-timer 0.05 nil 'invert-face 'mode-line))
        visible-bell t)

  ;; A combination of settings from prelude and minimal-emacs.d, may need tweaking
  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1
        hscroll-margin 2
        hscroll-step 1
        auto-window-vscroll nil)
  (setq pixel-scroll-precision-mode t)
  (setq-default word-wrap t
                truncate-lines t
                fill-column 80)
  ;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
  ;; when the window is narrower than `truncate-partial-width-windows' characters.
  (setq truncate-partial-width-windows nil)

  (setq use-short-answers t)

  (setq frame-title-format
        '("Emacs: " (:eval (if (buffer-file-name)
                               (abbreviate-file-name (buffer-file-name))
                             "%b"))))

  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'meta
          mac-right-option-modifier 'none)
    ;; After swapping control and command, this works nicely,
    ;; otherwise use (setq mac-command-modifier 'super)
    (setq mac-command-modifier 'control
          mac-control-modifier 'super)))

(use-feature view
  ;; http://yummymelon.com/devnull/enhancing-navigation-in-emacs-view-mode.html
  :hook
  (view-mode . (lambda ()
                 (cond ((derived-mode-p 'org-mode)
                        (define-key view-mode-map (kbd "p") 'org-previous-visible-heading)
                        (define-key view-mode-map (kbd "n") 'org-next-visible-heading))
                       ((derived-mode-p 'markdown-mode)
                        (define-key view-mode-map (kbd "p") 'markdown-outline-previous)
                        (define-key view-mode-map (kbd "n") 'markdown-outline-next))
                       ((derived-mode-p 'html-mode)
                        (define-key view-mode-map (kbd "p") 'sgml-skip-tag-backward)
                        (define-key view-mode-map (kbd "n") 'sgml-skip-tag-forward))
                       ((derived-mode-p 'emacs-lisp-mode)
                        (define-key view-mode-map (kbd "p") 'backward-sexp)
                        (define-key view-mode-map (kbd "n") 'forward-sexp))
                       ((derived-mode-p 'clojure-mode)
                        (define-key view-mode-map (kbd "p") 'backward-sexp)
                        (define-key view-mode-map (kbd "n") 'forward-sexp))
                       ((derived-mode-p 'makefile-mode)
                        (define-key view-mode-map (kbd "p") 'makefile-previous-dependency)
                        (define-key view-mode-map (kbd "n") 'makefile-next-dependency))
                       ((derived-mode-p 'c-mode)
                        (define-key view-mode-map (kbd "p") 'c-beginning-of-defun)
                        (define-key view-mode-map (kbd "n") 'c-end-of-defun))
                       (t
                        (define-key view-mode-map (kbd "p") 'scroll-down-command)
                        (define-key view-mode-map (kbd "n") 'scroll-up-command))))))

(use-package hl-todo
  :bind
  (:map hl-todo-mode-map
        ("C-c c t p" . hl-todo-previous)
        ("C-c c t n" . hl-todo-next)
        ("C-c c t o" . hl-todo-occur)
        ("C-c c t r" . hl-todo-rgrep)
        ("C-c c t i" . hl-todo-insert))
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . "red3")
     ("FIXME"  . "red3")
     ("DEBUG"  . "#A020F0")
     ("GOTCHA" . "#FF4500")
     ("HACK" . "#FF4500")
     ("STUB"   . "#1E90FF")
     ("FAIL"   . "red3")
     ("NOTE"   . "DarkOrange2")
     ("DEPRECATED" . "yellow")))
  :hook (elpaca-after-init . global-hl-todo-mode))

(use-feature whitespace
  :diminish
  :custom
  (whitespace-line-column 120)
  (whitespace-style '(face tabs empty trailing lines-tail))
  :config
  (defun turn-off-whitespace-mode ()
    (whitespace-mode -1))
  (defvar-local whitespace-disabled-modes '(cider-repl-mode ielm-mode vterm-mode eshell-mode shell-mode term-mode ansi-term-mode))
  (dolist (mode whitespace-disabled-modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'turn-off-whitespace-mode))
  :hook
  ((text-mode prog-mode) . whitespace-mode))

(use-package goggles
  :diminish
  :custom
  (goggles-pulse-delay 0.1)
  (goggles-pulse-iterations 15)
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (set-face-background 'goggles-removed "red4"))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-feature paren
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode +1))

(use-package which-key
  :defer 5
  :diminish
  :config (which-key-mode +1))

(use-package rainbow-delimiters
  :hook
  ((text-mode prog-mode ielm-mode) . #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :hook
  ((emacs-lisp-mode css-mode) . rainbow-mode))

(use-feature repeat
  :defer 5
  :config
  (let ((inhibit-message t))
    (repeat-mode +1))
  (defvar buffer-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<left>") #'previous-buffer)
      (define-key map (kbd "<right>") #'next-buffer)
      map))
  (dolist (cmd '(previous-buffer next-buffer))
    (put cmd 'repeat-map 'buffer-repeat-map)))

(use-package beacon
  :defer 5
  :diminish
  :custom
  (beacon-color "yellow")
  (beacon-push-mark 20)
  (beacon-blink-when-point-moves-vertically 10)
  (beacon-blink-when-focused t)
  :bind ("C-c c b" . beacon-blink)
  :config
  (append-to-list* 'beacon-dont-blink-major-modes 'cider-repl-mode 'eshell-mode 'shell-mode 'vterm-mode 'term-mode 'ansi-term-mode)
  (beacon-mode 1))

(use-package helpful
  :bind (("C-h f" . helpful-function)
         ("C-h F" . helpful-callable)
         ("C-h x" . helpful-command)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h o" . helpful-symbol)
         ("C-h C-." . helpful-at-point)))

(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    ;; loading emacs-lisp-mode breaks keybindings, so just setup syntax-table/completion
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (add-hook 'completion-at-point-functions #'elisp-completion-at-point nil t)

    ;; Run setup hook for `eval-expression' (calls `eldoc--eval-expression-setup')
    (run-hooks 'eval-expression-minibuffer-setup-hook)

    (mono-complete-mode +1)
    (setq-local mono-complete-fallback-command 'dabbrev-expand)

    ;; smartparens, but don't insert pairs of '
    (smartparens-strict-mode)
    (setq-local sp-pair-list (assoc-delete-all "'" sp-pair-list))
    (setq-local sp-local-pairs (seq-filter '(lambda (x) (not (string= "'" (cadr x)))) sp-local-pairs))))

(use-package highlight-sexp
  :ensure (highlight-sexp :host github :repo "daimrod/highlight-sexp")
  :diminish
  ;; TODO grey8 ?
  :custom (hl-sexp-background-color "grey10")
  :hook ((emacs-lisp-mode clojure-mode) . highlight-sexp-mode))

(use-package highlight-indent-guides
  :diminish
  :custom (highlight-indent-guides-method 'character)
  :config
  ;; TODO can we do the same with highlight-indent-guides-auto-* ?
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "grey25")
  (set-face-background 'highlight-indent-guides-even-face "grey25")
  (set-face-foreground 'highlight-indent-guides-character-face "grey25")
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode))

;; Main causal collection package; the individual packages are configured in the appropriate places
(use-package casual)

(provide 'init-ui)
;;; init-ui.el ends here
