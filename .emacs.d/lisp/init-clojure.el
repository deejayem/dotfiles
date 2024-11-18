;;; init-clojure.el --- Clojure Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;(require 'init-lisp)

(use-package yasnippet
  :diminish yas-minor-mode)

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  (define-abbrev-table 'clojure-mode-abbrev-table
    '(("scs" "#sc/spy")
      ("scp" "#spy/p")
      ("scd" "#spy/d")
      ("sct" "#spy/t")
      ("ms" "(miracle.save/save)" backward-char)
      ("ddb" "#d/dbg")
      ("db" "#d/dbg")
      ("dbn" "#d/dbgn")
      ("ddn" "#d/dbgn")
      ("sc" "#sc/spy")
      ("sd" "#spy/d")
      ("sp" "#spy/p")
      ("st" "#spy/t")))

  (defalias 'cape-clojure (cape-capf-super #'cider-complete-at-point
                                           #'lsp-completion-at-point
                                           #'cape-dabbrev))
  (defun set-clojure-capf ()
    (add-hook 'completion-at-point-functions #'cape-clojure -99 t))

  (defun clojure-mode-hook-fn ()
    (set-clojure-capf)
    (subword-mode +1))

  ;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
  (define-clojure-indent
   (defroutes 'defun)
   (GET 2)
   (POST 2)
   (PUT 2)
   (DELETE 2)
   (HEAD 2)
   (ANY 2)
   (OPTIONS 2)
   (PATCH 2)
   (rfn 2)
   (let-routes 1)
   (context 2))

  ;; https://github.com/magnars/.emacs.d/blob/04426922530edc3ebe9bae7a86632e3b1956049d/settings/setup-clojure-mode.el#L494-L537
  (defun my/cider-looking-at-lets? ()
    (or (looking-at "(let ")
        (looking-at "(letfn ")
        (looking-at "(when-let ")
        (looking-at "(if-let ")))

  (defun my/cider-collect-lets (&optional max-point)
    (let* ((beg-of-defun (save-excursion (beginning-of-defun) (point)))
           (lets nil))
      (save-excursion
        (while (not (= (point) beg-of-defun))
          (paredit-backward-up 1)
          (when (my/cider-looking-at-lets?)
            (save-excursion
              (let ((beg (point)))
                (paredit-forward-down 1)
                (paredit-forward 2)
                (when (and max-point (< max-point (point)))
                  (goto-char max-point))
                (setq lets (cons (concat (buffer-substring-no-properties beg (point))
                                         (if max-point "]" ""))
                                 lets))))))
        lets)))

  (defun my/inside-let-block? ()
    (save-excursion
      (paredit-backward-up 2)
      (my/cider-looking-at-lets?)))

  (defun my/cider-eval-including-lets (&optional output-to-current-buffer)
    "Evaluates the current sexp form, wrapped in all parent lets."
    (interactive "P")
    (let* ((beg-of-sexp (save-excursion (paredit-backward 1) (point)))
           (code (buffer-substring-no-properties beg-of-sexp (point)))
           (lets (my/cider-collect-lets (when (my/inside-let-block?)
                                          (save-excursion (paredit-backward 2) (point)))))
           (code (concat (s-join " " lets)
                         " " code
                         (s-repeat (length lets) ")"))))
      (cider-interactive-eval code
                              (when output-to-current-buffer
                                (cider-eval-print-handler))
                              nil
                              (cider--nrepl-pr-request-map))))


  :init
  ;; Always show more of the path in clj buffer names.
  ;; Using setq-local in clojure-mode-hook is not enough, as it runs too late
  (defun clj-uniquify-get-proposed-name (orig base dirname &optional depth original-dirname)
    (when (and (> (length base) 4)
               (string= ".clj" (substring base -4))
               (not (string= "project.clj" base)))
      (setq-local uniquify-min-dir-content 3))
    (funcall orig base dirname depth original-dirname))
  (advice-add 'uniquify-get-proposed-name :around 'clj-uniquify-get-proposed-name)
  :hook
  (clojure-mode . clojure-mode-hook-fn))

(use-package clj-refactor
  :diminish
  :bind ("C-c @" . hydra-cljr-help-menu/body)
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-add-ns-to-blank-clj-files nil) ; disable clj-refactor adding ns to blank files
  (cljr-slash-uses-suggest-libspec t)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (defun clj-refactor-hook-fn ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1))
  :hook
  (clojure-mode . clj-refactor-hook-fn))

(use-package cider
  :diminish
  :config
  (cider-enable-flex-completion)
  (defvar cider-main-function "-main")
  (defun cider-repl-mode-hook-fn ()
    (display-line-numbers-mode -1)
    (subword-mode +1))
  (defun cider-mode-hook-fn ()
    (eldoc-mode 1)
    ;; Remove this, as we use cape-clojure (in init-clojure.el), which includes
    ;; cider-complete-at-point
    (remove-hook 'completion-at-point-functions #'cider-complete-at-point t))
  (defun run-and-unhook ()
    (remove-hook 'cider-connected-hook 'run-and-unhook)
    (run-main))
  (defun run-main ()
    (interactive)
    (cider-insert-in-repl (concat "(" cider-main-function ")") t))
  (defun cider-jack-in-and-run-main (arg &rest params)
    (interactive "P")
    (add-hook 'cider-connected-hook 'run-and-unhook)
    (cider-jack-in params))
  (defun load-debug-namespaces ()
    (interactive)
    (cider-interactive-eval "(require 'snitch.core)" nil nil (cider--nrepl-pr-request-map))
    (cider-interactive-eval "(require 'miracle.save)" nil nil (cider--nrepl-pr-request-map))
    (cider-interactive-eval "(require 'sc.api)" nil nil (cider--nrepl-pr-request-map))
    (cider-interactive-eval "(require '[debux.cs.core :refer [dbg dbgn dbgt]])" nil nil (cider--nrepl-pr-request-map)))
  (defun cider-toggle-boolean ()
    (interactive)
    (let ((opposite (pcase (cider-symbol-at-point)
                      ("false" "true")
                      ("true" "false"))))
      (when opposite
        (pcase-let ((`(,start . ,end) (bounds-of-thing-at-point 'symbol)))
          (delete-region start end))
        (insert opposite))))
  (define-abbrev-table 'cider-repl-mode-abbrev-table
    '(("scl" "(eval `(sc.api/defsc ~(sc.api/last-ep-id)))" cider-repl-return)
      ("scs" "(sc.api/defsc*)" cider-repl-return)
      ("scd" "(sc.api/defsc)" backward-char)
      ("ms" "(use 'miracle.save)" cider-repl-return)
      ("ld" "(ld)" backward-char)
      ("ps" "(print-saves)" backward-char)
      ("sv" "(save-var*)" backward-char)
      ("usv" "(unsave-var*)" backward-char)
      ("sn" "(save-ns*)" backward-char)
      ("usn" "(unsave-ns*)" backward-char)
      ("fs" "@f-saves")
      ("ldf" "(ld)" backward-char)
      ("pfs" "(print-f-saves)" backward-char)
      ("scr" "(use 'spyscope.repl)" cider-repl-return)))
  (setq cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-display-help-banner nil
        cider-repl-history-highlight-current-entry t
        cider-repl-history-highlight-inserted-item t
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-save-file-on-load t
        cider-test-show-report-on-success t
        ;; cider-invert-insert-eval-p t
        ;; cider-switch-to-repl-on-insert nil
        cider-xref-fn-depth 90
        cider-repl-history-file ".cider-repl-history"
        ;; nrepl-log-messages t
        cider-connection-message-fn nil
        cider-show-error-buffer 'except-in-repl
        cider-test-fail-fast nil
        clojure-toplevel-inside-comment-form t)
  (setq cider-clojure-compilation-error-phases nil)
  (setq-default cider-use-overlays t)
  (unbind-key "C-c C-l" cider-mode-map)
  (unbind-key "C-c C-b" cider-mode-map)
  (unbind-key "C-c C-b" cider-repl-mode-map)

  (defun fix-duplicate-windows ()
    "When all windows are the same, delete all of them except the current one."
    (when (apply #'eq (mapcar 'window-buffer (window-list)))
      (delete-other-windows)))
  (advice-add #'cider-close-ancillary-buffers :after #'fix-duplicate-windows)

  :bind
  (:map cider-mode-map
        ("C-c M-l" . cider-load-file)
        ("C-c M-b" . cider-interrupt)
        ("C-x M-i e" . cider-inspect-last-sexp)
        ("C-x M-i f" . cider-inspect-defun-at-point)
        ("C-x M-i l" . cider-inspect-last-result)
        ("C-x M-i v" . cider-inspect-expr))
  (:map cider-repl-mode-map
        ("C-c M-b" . cider-interrupt)
        ;; sp commands sometimes behave strangely in the cider repl buffer
        ("M-d" . paredit-forward-kill-word)
        ("M-DEL" . paredit-backward-kill-word)
        ("C-k" . paredit-kill)
        ("C-x M-i e" . cider-inspect-last-sexp)
        ("C-x M-i f" . cider-inspect-defun-at-point)
        ("C-x M-i l" . cider-inspect-last-result)
        ("C-x M-i v" . cider-inspect-expr))
  (:map cider-start-map
        ("C-c C-M-j" . cider-jack-in-and-run-main))
  (:map clojure-mode-map
        ("C-c C-r C-m" . run-main)
        ("C-c C-r C-d" . load-debug-namespaces)
        ("C-c C-M-j" . cider-jack-in-and-run-main)
        ("C-x p q" . project-clojure-test-switch)
        ("C-c C-M-c" . (lambda () (interactive) (cider-clear-compilation-highlights t)))
        ("C-c C->" . cider-find-dwim)
        ("C-x 4 C->" . cider-find-dwim-other-window))
  :hook
  (cider-repl-mode . cider-repl-mode-hook-fn)
  (cider-mode . cider-mode-hook-fn))

(use-package babashka)

(use-package jet
  :bind ("C-c j" . jet))

(provide 'init-clojure)
;;; init-clojure.el ends here
