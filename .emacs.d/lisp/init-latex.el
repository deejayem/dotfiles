;;; init-latex.el --- LaTeX Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Based on prelude-latex.el
;;; Code:

;; (use-package auctex
;;   :config
;;   (require 'smartparens-latex)
;;   (setq-default TeX-master nil)
;;   (when (eq system-type 'darwin)
;;     (setq TeX-view-program-selection
;;           '((output-dvi "DVI Viewer")
;;             (output-pdf "PDF Viewer")
;;             (output-html "HTML Viewer")))
;;     (setq TeX-view-program-list
;;           '(("DVI Viewer" "open %o")
;;             ("PDF Viewer" "open %o")
;;             ("HTML Viewer" "open %o"))))
;;   :custom
;;   (TeX-auto-save t)
;;   (TeX-parse-qself t)
;;   (TeX-close-quote "")
;;   (TeX-open-quote "")
;;   ;; use pdflatex
;;   (TeX-PDF-mode t)
;;   :hook LaTeX-mode . (lambda ()
;;                        (abbrev-mode +1)
;;                        (turn-on-auto-fill)
;;                        ;; (LaTeX-math-mode (LaTeX-math-mode 1)) ;; alternative to the below
;;                        (turn-on-cdlatex)))
;; 
;; (use-package cdlatex)

(provide 'init-latex)
;;; init-latex.el ends here
