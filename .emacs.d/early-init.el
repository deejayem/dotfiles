;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(setq frame-inhibit-implied-resize t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setq package-enable-at-startup nil)

; (setq comp-deferred-compilation nil)

;;; early-init.el ends here
