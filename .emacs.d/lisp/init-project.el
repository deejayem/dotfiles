;;; init-project.el --- Project (and Perspective) Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(use-package project
  :ensure nil
  :config
  (defun project--clojure-switch-to-test (filename project-root)
    (let* ((project-src-file (string-remove-prefix project-root filename))
           (project-test-file (replace-regexp-in-string "\.clj$" "_test.clj"
                                                (replace-regexp-in-string "^src/" "test/" project-src-file))))
      (find-file (expand-file-name project-test-file project-root))))
  (defun project--clojure-switch-to-src (test-filename project-root)
    (let* ((project-test-file (string-remove-prefix project-root test-filename))
           (project-src-file (replace-regexp-in-string "_test\.clj$" ".clj"
                                                        (replace-regexp-in-string "^test/" "src/" project-test-file))))
      (find-file (expand-file-name project-src-file project-root))))
  (defun project-clojure-test-switch ()
    (interactive)
    (let ((filename (buffer-file-name))
          (project-root (expand-file-name (project-root (project-current)))))
      (cond ((string-match (concat "^" project-root "test/.*_test\.clj") filename)
             (project--clojure-switch-to-src filename project-root))
            ((string-match (concat "^" project-root "src/.*\.clj") filename)
             (project--clojure-switch-to-test filename project-root)))))
  (defun project-recentf ()
    "Show a list of recently visited files in a project."
    (interactive)
    (if (boundp 'recentf-list)
        (let* ((project-root (expand-file-name (project-root (project-current))))
               (project-recentf-files (mapcar
                                       (lambda (f) (file-relative-name f project-root))
                                       (seq-filter (apply-partially 'string-prefix-p project-root) recentf-list))))
          (find-file (expand-file-name
                      (funcall project-read-file-name-function
                               "Find recent project files"
                               project-recentf-files nil 'file-name-history nil)
                      project-root)))
      (message "recentf is not enabled")))

  (add-to-list 'project-switch-commands '(?h "Recentf" project-recentf) t)
  (add-to-list 'project-switch-commands '(?b "Consult Project Buffer" consult-project-buffer) t)
  (add-to-list 'project-switch-commands '(?r "Consult Ripgrep" consult-ripgrep) t)
  (add-to-list 'project-switch-commands '(?m "Magit" magit-status) t)
  (add-to-list 'project-switch-commands '(?R "Replace Regexp" project-query-replace-regexp) t)

  ;; project-root and project-try-local copied/modified from https://github.com/karthink/project-x/blob/master/project-x.el
  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))
  (defun project-try-local (dir)
    "Treat DIR as a project if it contains a .project file."
    (if-let ((root (locate-dominating-file dir ".project")))
        (cons 'local root)))
  ;; Add this hook last so so that vc takes precedence over local
  (add-hook 'project-find-functions 'project-try-local 90)
  :bind
  ("C-x p P" . project-switch-project)
  ("C-x f" . project-recentf))

(use-package perspective
  ;; Don't restart persp-mode when re-evaluating, as it clears the current persp list
  :init (or persp-mode (persp-mode))
  :config
  (defun switch-project (proj)
    "Switch to project or already open project perspective."
    (interactive (list (project-prompt-project-dir)))
    (let* ((persp-name (file-name-nondirectory (directory-file-name proj)))
           (persp (gethash persp-name (perspectives-hash))))
      (unless (equal persp (persp-curr))
        ;; Create or switch to a perspective named after the project
        (persp-switch persp-name)
        ;; If the perspective did not exist, switch to the project
        (when (not persp)
          (project-switch-project proj)))))
  :bind
  ("C-x p p" . switch-project)
  ("C-x x x" . persp-switch-quick))

(provide 'init-project)
;;; init-project.el ends here
