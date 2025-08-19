;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when (eq system-type 'darwin)
  (setq frame-resize-pixelwise t))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq load-prefer-newer t
      native-comp-async-report-warnings-errors nil
      native-comp-warning-on-missing-source nil
      warning-suppress-log-types '((comp) (bytecomp))
      byte-compile-verbose nil
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(setq frame-inhibit-implied-resize t)

(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(undecorated-round . t))
  (toggle-frame-maximized))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (horizontal-scroll-bar-mode -1))

;; Copied from https://github.com/jamescherti/minimal-emacs.d
;; Some features that are not represented as packages can be found in
;; `features', but this can be inconsistent. The following enforce consistency:
(if (fboundp #'json-parse-string)
    (push 'jansson features))
(if (string-match-p "HARFBUZZ" system-configuration-features) ; no alternative
    (push 'harfbuzz features))
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))

(setq package-enable-at-startup nil)

;; Some optimizations from doom.el (some of these probably don't belong here!)
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq idle-update-delay 1.0)
(setq inhibit-compacting-font-caches t)
(setq redisplay-skip-fontification-on-input t)

;; Copied/modified from https://github.com/jamescherti/minimal-emacs.d
(setq ad-redefinition-action 'accept
      warning-suppress-types '((lexical-binding))
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      use-file-dialog nil
      use-dialog-box nil)
(advice-add #'display-startup-screen :override #'ignore)
(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))
(unless (memq initial-window-system '(x pgtk))
  (setq command-line-x-option-alist nil))

(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))
(define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
  (advice-remove #'load-file #'load-file@silence))

;;; early-init.el ends here
