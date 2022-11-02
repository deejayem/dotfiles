;;; init-ui.el --- UI Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Some parts copied from prelude-ui.el and prelude-editor.el
;; Frame customisations, and disabling of {menu,tool,scoll}-bar-mode done in early-init.el, rather than here
;;; Code:

(use-package emacs
  :hook (emacs-startup . (lambda ()
                           (cond
                            ((find-font (font-spec :name "iosevka comfy"))
                             (set-face-attribute 'default nil :font "iosevka comfy"))
                            ((find-font (font-spec :name "iosevka"))
                             (set-face-attribute 'default nil :font "iosevka")))

                           (global-display-line-numbers-mode)
                           (global-hl-line-mode +1)

                           (global-set-key (kbd "C-x C-S-k") 'kill-this-buffer)))

  :config
  ;; https://github.com/rougier/elegant-emacs/blob/master/sanity.el
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        inhibit-startup-message t
        initial-scratch-message nil)

  (blink-cursor-mode -1)
  (setq-default cursor-type 'bar)

  (setq whitespace-line-column 120)
  (setq show-trailing-whitespace t)
  (setq-default indicate-empty-lines t)
  (setq ring-bell-function 'ignore
        visible-bell t)

  ;; TODO do we want these? (copied from prelude)
  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq frame-title-format
        '("Emacs: " (:eval (if (buffer-file-name)
                               (abbreviate-file-name (buffer-file-name))
                             "%b"))))

  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'meta)
    (setq mac-right-option-modifier 'none)
    (setq mac-command-modifier 'super)))

(use-package modus-themes
  :config
  (defun my/load-theme ()
    "Load modus vivendi theme, with my customisations."
    (setq modus-themes-syntax '(green-strings yellow-comments)
          modus-themes-paren-match '(bold intense underline)
          modus-themes-bold-constructs t
          modus-themes-italic-constructs t
          modus-themes-lang-checkers '(text-also))
    (load-theme 'modus-vivendi t)

    (custom-set-faces
     `(font-lock-builtin-face ((t (:foreground "LawnGreen"))))
     `(font-lock-keyword-face ((t (:foreground "gold"))))
     `(font-lock-function-name-face ((t (:foreground "cyan"))))
     `(font-lock-variable-name-face ((t (:foreground "gold3"))))
     `(font-lock-constant-face ((t (:foreground "DeepSkyBlue2"))))
     `(font-lock-type-face ((t (:foreground "PaleGreen2"))))
     `(font-lock-string-face ((t (:foreground "SpringGreen3"))))
     `(font-lock-comment-face ((t (:foreground "burlywood"))))
     `(font-lock-doc-face ((t :foreground "LightCyan3")))
     `(region ((t (:background "firebrick"))))
     `(secondary-selection ((t (:background "firebrick4"))))
     `(highlight ((t (:background "grey30"))))
     `(idle-highlight ((t (:background "grey50" :foreground "white"))))
     `(isearch ((t (:background "coral2"))))
     `(lazy-highlight ((t (:background "LightSteelBlue2" :foreground "black"))))
     `(match ((t (:background "gray35" :foreground "grey85"))))
     `(lsp-face-highlight-textual ((t (:background "DimGrey"))))
     `(whitespace-empty ((t (:background "gray10"))))
     `(hl-line ((t :background "gray20" :underline "gray40" :inherit nil)))
     `(simple-modeline-status-modified ((t :foreground "DeepSkyBlue")))
     `(consult-async-split ((t :foreground "LightCoral")))
     `(orderless-match-face-0 ((t :foreground "tomato")))
     `(orderless-match-face-1 ((t :foreground "SpringGreen2")))
     `(orderless-match-face-2 ((t :foreground "gold")))
     `(orderless-match-face-3 ((t :foreground "cyan")))
     `(flycheck-fringe-warning ((t :foreground "white" :background "gold3")))
     `(flycheck-fringe-error ((t :foreground "white" :background "red2")))
     `(flycheck-fringe-info ((t :foreground "white" :background "RoyalBlue3")))
     `(alt-font-lock-keyword-face ((t :foreground "LightSkyBlue" :weight bold)))
     `(alt-hl-line-face ((t :underline "gray50" :weight bold))))

    (setq hl-todo-keyword-faces
          '(("TODO"   . "red3")
            ("FIXME"  . "red3")
            ("DEBUG"  . "#A020F0")
            ("GOTCHA" . "#FF4500")
            ("HACK" . "#FF4500")
            ("STUB"   . "#1E90FF")
            ("FAIL"   . "red3")
            ("NOTE"   . "DarkOrange2")
            ("DEPRECATED" . "yellow"))))

  (defun use-alt-font-lock-keyword-face ()
    "Remap font-lock-keyword-face to the alternate one, in the current buffer"
    (face-remap-add-relative 'font-lock-keyword-face 'alt-font-lock-keyword-face))
  (defun use-alt-hl-line-face ()
    "Remap hl-line face to the alternate one, in the current buffer"
    (face-remap-add-relative 'hl-line 'alt-hl-line-face))
  :hook
  (after-init . my/load-theme)
  (cider-inspector-mode . use-alt-font-lock-keyword-face)
  (magit-mode . use-alt-hl-line-face))

(use-package hl-todo
  :bind
  (:map hl-todo-mode-map
        ("C-c c t p" . hl-todo-previous)
        ("C-c c t n" . hl-todo-next)
        ("C-c c t o" . hl-todo-occur)
        ("C-c c t r" . hl-todo-rgrep)
        ("C-c c t i" . hl-todo-insert))
  ;; Use emacs-startup-hook so that it runs after my/load-theme is called in after-init-hook
  ;; hl-todo-keyword-faces is customised in my/load-theme
  :hook (emacs-startup . global-hl-todo-mode))

(use-package whitespace
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
  (text-mode . (lambda () (whitespace-mode +1)))
  (prog-mode . (lambda () (whitespace-mode +1))))

(use-package volatile-highlights
  :defer 10
  :diminish
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  :init
  (volatile-highlights-mode t))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package which-key
  :defer 5
  :diminish
  :config (which-key-mode +1))

(use-package rainbow-delimiters
  :hook
  (text-mode . (lambda () (rainbow-delimiters-mode +1)))
  (prog-mode . (lambda () (rainbow-delimiters-mode +1))))

(use-package rainbow-mode
  :diminish)

(use-package repeat
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
  (beacon-blink-duration 0.4)
  (beacon-size 45)
  ;; (beacon-blink-when-point-moves-vertically t) ;; TODO why does this cause errors?
  (beacon-blink-when-focused t)
  :bind ("C-c c b" . beacon-blink)
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'cider-repl-mode t)
  (beacon-mode 1))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h C-f" . helpful-command)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h o" . helpful-symbol)
         ("C-h C-." . helpful-at-point)))

;; From https://github.com/jwiegley/dot-emacs/blob/master/init.el
(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (smartparens-strict-mode)))

(provide 'init-ui)
;;; init-ui.el ends here
