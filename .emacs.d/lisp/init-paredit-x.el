;;; init-paredit-x.el --- Paredit Extra Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Code taken from emacswiki and https://github.com/bodil/emacs.d/blob/master/bodil/bodil-paredit.el
;;; Code:

(with-eval-after-load 'paredit
;; From emacswiki
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
           "(foo bar baz (|quux) zot)"))))

(paredit-define-keys)
(paredit-annotate-mode-with-examples)
(paredit-annotate-functions-with-examples)

;; From https://github.com/bodil/emacs.d/blob/master/bodil/bodil-paredit.el
;; Inverse M-(
(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))
(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd "M-)")
     'paredit-wrap-round-from-behind))

;; From https://github.com/bodil/emacs.d/blob/master/bodil/bodil-paredit.el
;; Duplicate sexp
(defun paredit-duplicate-after-point
  ()
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
  (exchange-point-and-mark)))
(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd "C-c C-S-d")
     'paredit-duplicate-after-point))

(provide 'init-paredit-x)
;;; init-paredit-x.el ends here
