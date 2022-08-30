;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq load-prefer-newer nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(setq frame-inhibit-implied-resize t)

(toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setq package-enable-at-startup nil)

;; Some optimizations from doom.el (some of these probably don't belong here!)
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq ffap-machine-p-known 'reject)
(setq idle-update-delay 1.0)
(setq inhibit-compacting-font-caches t)
(setq redisplay-skip-fontification-on-input t)

(define-advice load-file (:override (file) silence)
  (load file nil :nomessage))

;;; early-init.el ends here
