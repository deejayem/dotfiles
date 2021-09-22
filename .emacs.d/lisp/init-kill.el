;;; init-kill.el --- Kill Ring Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Contains code copied from prelude-editor.el
;;; Code:

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package emacs
  :config
  (defadvice exchange-point-and-mark (before deactivate-mark activate compile)
    "When called with no active region, do not activate mark."
    (interactive
     (list (not (region-active-p)))))

  (defun yank-advised-indent-function (beg end)
    "Do indentation, as long as the region isn't too large."
    (if (<= (- end beg) 1000)
        (indent-region beg end nil)))

  (defmacro advise-commands (advice-name commands class &rest body)
    "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
    `(progn
       ,@(mapcar (lambda (command)
                   `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                      ,@body))
                 commands)))

  (advise-commands "indent" (yank yank-pop) after
                   (if (and (not (ad-get-arg 0))
                            (not (member major-mode '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)))
                            (or (derived-mode-p 'prog-mode)
                                (member major-mode '(LaTeX-mode TeX-mode))))
                       (let ((transient-mark-mode nil))
                         (yank-advised-indent-function (region-beginning) (region-end))))))

(provide 'init-kill)
;;; init-kill.el ends here
