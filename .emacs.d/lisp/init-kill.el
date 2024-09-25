;;; init-kill.el --- Kill Ring Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Contains code copied from prelude-editor.el
;;; Code:

(use-package browse-kill-ring
  :defer 5
  :config
  (browse-kill-ring-default-keybindings))

(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ;; emulate expand-region
  ("C-=" . easy-mark)
  (:map easy-kill-base-map ("C-=" . easy-kill-expand)))

(use-feature emacs
  :custom (kill-do-not-save-duplicates t)
  :hook
  (elpaca-after-init . (lambda ()
                         ;; Based on code in prelude-editor.el
                         (defun yank-advised-indent-function (beg end)
                           "Do indentation, as long as the region isn't too large."
                           (if (<= (- end beg) 10000)
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
                                                (yank-advised-indent-function (region-beginning) (region-end))))))))

(provide 'init-kill)
;;; init-kill.el ends here
