;;; init-paredit.el --- Paredit Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; add-hooks/add-lisp-hook based on https://github.com/bodil/emacs.d/blob/master/bodil/bodil-lisp.el
;;; Code:

(defvar-local lisp-modes
 '(scheme-mode emacs-lisp-mode lisp-mode clojure-mode cider-repl-mode
   eval-expression-minibuffer-setup ielm-mode lisp-interaction-mode))

(use-package emacs
  :config
  (defun add-hooks (modes func)
    (dolist (mode modes)
      (add-hook (intern (concat (symbol-name mode) "-hook")) func)))
  (defun add-lisp-hook (func)
    (add-hooks lisp-modes func)))

(use-package paredit
  :diminish
  :bind
  (:map paredit-mode-map
        ("M-[" . paredit-smart-wrap-square)
        ("C-c M-{" . paredit-smart-wrap-curly)
        ("C-c M-<" . paredit-smart-wrap-angled)
        ([remap paredit-wrap-round] . paredit-smart-wrap-round)
        ([remap paredit-meta-doublequote] . paredit-smart-metadouble-quote)
        ("M-W" . paredit-copy-as-kill))
  :config
  (defmacro define-paredit-smart-wrap (name)
    `(defun ,(paredit-conc-name "paredit-smart-wrap-" name)
         (&optional argument)
       ,(concat "Wrap the following S-expression, from the beginning of the current symbol.
See `paredit-wrap-sexp' for more details.
Falls back to smartparens in comments and strings.")
       (interactive "P")
       (if (or (paredit-in-string-p)
               (paredit-in-comment-p)
               (paredit-in-char-p))
           (,(intern (concat "sp-wrap-" name)))
         (beginning-of-thing 'symbol)
         (,(intern (concat "paredit-wrap-" name)) argument))))

  (define-paredit-smart-wrap "round")
  (define-paredit-smart-wrap "curly")
  (define-paredit-smart-wrap "square")
  (define-paredit-smart-wrap "angled")

  (defun sp-wrap-double-quotation-marks ()
    (interactive)
    (sp-wrap-with-pair "\""))

  ;; paredit-meta-doublequote is not like the wrap functions (but can act as one)
  (defun paredit-smart-metadouble-quote (&optional n)
    "Move to the end of the string.
If not in a string, act as `paredit-doublequote'; if not prefix argument
 is specified and the region is not active or `transient-mark-mode' is
 disabled, the default is to wrap one S-expression, however, not zero.
If wrapping, move to the beginning of the symbol first.
Falls back to smartparens in comments."
    (interactive "P")
    (if (paredit-in-comment-p)
        (sp-wrap-double-quotation-marks)
      (when (not (paredit-in-string-p))
        (beginning-of-thing 'symbol))
      (paredit-meta-doublequote n)))

  :init
  ;; From emacswiki - extreme barfage & slurpage
  (defun paredit-barf-all-the-way-backward ()
    (interactive)
    (paredit-split-sexp)
    (paredit-backward-down)
    (paredit-splice-sexp))
  (defun paredit-barf-all-the-way-forward ()
    (interactive)
    (paredit-split-sexp)
    (paredit-forward-down)
    (paredit-splice-sexp)
    (if (eolp) (delete-horizontal-space)))
  (defun paredit-slurp-all-the-way-backward ()
    (interactive)
    (catch 'done
      (while (not (bobp))
        (save-excursion
          (paredit-backward-up)
          (if (eq (char-before) ?\()
              (throw 'done t)))
        (paredit-backward-slurp-sexp))))
  (defun paredit-slurp-all-the-way-forward ()
    (interactive)
    (catch 'done
      (while (not (eobp))
        (save-excursion
          (paredit-forward-up)
          (if (eq (char-after) ?\))
              (throw 'done t)))
        (paredit-forward-slurp-sexp))))

  ;; From https://github.com/bodil/emacs.d/blob/master/bodil/bodil-paredit.el
  ;; Inverse M-(
  (defun paredit-wrap-round-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-round)
    (insert " ")
    (forward-char -1))
  ;; From https://github.com/bodil/emacs.d/blob/master/bodil/bodil-paredit.el
  ;; Duplicate sexp
  (defun paredit-duplicate-after-point ()
    "Duplicates the content of the line that is after the point."
    (interactive)
    ;; skips to the next sexp
    (while (looking-at " ")
      (forward-char))
    (set-mark-command nil)
    ;; while we find sexps we move forward on the line
    (while (and (<= (point) (car (bounds-of-thing-at-point 'sexp)))
                (not (= (point) (line-end-position))))
      (forward-sexp)
      (while (looking-at " ")
        (forward-char)))
    (kill-ring-save (mark) (point))
    ;; go to the next line and copy the sexprs we encountered
    (paredit-newline)
    (set-mark-command nil)
    (yank)
    (exchange-point-and-mark))

  (defun replace-paredit-binding (commands original-keys new-keys)
    (when commands
      (if (and (consp (car commands))
               (equal (caar commands) original-keys))
          (setcar (car commands) new-keys)
        (replace-paredit-binding (cdr commands) original-keys new-keys))))
  (defun paredit-commands-advice ()
    ;; Default paredit-convolute-sexp binding clashes with xref-find-references
    (replace-paredit-binding paredit-commands "M-?" "C-c M-?")

    (nconc paredit-commands
           '("Extreme Barfage & Slurpage"
             (("C-M-)")
              paredit-slurp-all-the-way-forward
              ("(foo (bar |baz) quux zot)"
               "(foo (bar |baz quux zot))")
              ("(a b ((c| d)) e f)"
               "(a b ((c| d)) e f)"))
             (("C-M-}" "M-F")
              paredit-barf-all-the-way-forward
              ("(foo (bar |baz quux) zot)"
               "(foo (bar|) baz quux zot)"))
             (("C-M-(")
              paredit-slurp-all-the-way-backward
              ("(foo bar (baz| quux) zot)"
               "((foo bar baz| quux) zot)")
              ("(a b ((c| d)) e f)"
               "(a b ((c| d)) e f)"))
             (("C-M-{" "M-B")
              paredit-barf-all-the-way-backward
              ("(foo (bar baz |quux) zot)"
               "(foo bar baz (|quux) zot)")))
           '("Extra"
             (("C-c M-)")
              paredit-wrap-round-from-behind
              ("(foo| bar baz" "((| foo) bar baz"))
             (("C-c C-S-d")
              paredit-duplicate-after-point
              ("|(foo)" "(foo)\n|(foo)"))))

    ;; Only need to do this once
    (advice-remove 'paredit-define-keys 'paredit-commands-advice))

  (advice-add 'paredit-define-keys :before 'paredit-commands-advice)
  (add-lisp-hook #'turn-off-smartparens-mode)
  (add-lisp-hook #'enable-paredit-mode))

(provide 'init-paredit)
;;; init-paredit.el ends here
