;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

; (setq comp-deferred-compilation nil)

;;; early-init.el ends here
