;;; init-git.el --- VCS/Git Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ediff
  :custom
  (ediff-setup-windows-plain 'ediff-setup-windows-plain))

(use-package diff-hl
  :hook
  (emacs-startup . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package gitconfig)
(use-package git-modes)

(use-package git-timemachine
  :bind
  ("C-x v t" . git-timemachine-toggle))

(use-package vc
  :bind
  (("C-x v C-r" . my/vc-refresh-state)
   ("C-x v C-m" . my/update-git-master))
  :custom (vc-follow-symlinks nil)
  :config
  (defun my/vc-refresh-state ()
    (interactive)
    (when-let ((root-dir (vc-root-dir)))
      (dolist (buf (buffer-list))
        (when (and (not (buffer-modified-p buf))
                   (buffer-file-name buf)
                   (file-exists-p (buffer-file-name buf))
                   (file-in-directory-p (buffer-file-name buf) root-dir))
          (with-current-buffer buf
            (vc-refresh-state))))))

  ;; [alias]
  ;;   update-master = !git fetch origin master:master
  ;;   update-main = !git fetch origin main:main
  (defun my/update-git-master ()
    "Update git master or main branch."
    (interactive)
    (if-let ((root (vc-root-dir)))
        (let* ((branches (vc-git-branches))
               (main-p (member "main" branches))
               (master-p (member "master" branches))
               (current-branch (car branches))
               (on-master-p (member current-branch '("master" "main")))
               (command (if main-p "update-main" "update-master"))
               (buffer "*vc-update-master*"))
          (if on-master-p
              (vc-pull)
            ;; based on vc-git--pushpull
            (require 'vc-dispatcher)
            (apply #'vc-do-async-command buffer root vc-git-program command nil)
            (with-current-buffer buffer
              (vc-run-delayed
                (vc-compilation-mode 'git)
                (setq-local compile-command
                            (concat vc-git-program " " command))
                (setq-local compilation-directory root)
                (setq-local compilation-arguments
                            (list compile-command nil
                                  (lambda (_name-of-mode) buffer)
                                  nil))))
            (vc-set-async-update buffer)))
      (message "not a git repository"))))

(use-package magit
  :bind
  ("C-c g g" . magit-dispatch) ;; magit-file-dispatch is C-c M-g
  ("C-c g u" . my/magit-set-upstream)
  ("C-c g r" . my/magit-refresh-state)
  ("C-c g m" . my/magit-update-git-master)
  :config
  ;; Requires the following gitconfig:
  ;; [alias]
  ;;   upstream = !git push -u origin HEAD
  ;; TODO - this is useful after setting push remote, but is there a better way?
  (defun my/magit-set-upstream ()
    (interactive)
    (magit-shell-command-topdir "git upstream"))

  ;; update stale git info on the modeline (based on code removed from doom modeline)
  (defun my/magit-refresh-state ()
    "Update modeline git branch information."
    (interactive)
    (dolist (buf (buffer-list))
      (when (and (not (buffer-modified-p buf))
                 (buffer-file-name buf)
                 (file-exists-p (buffer-file-name buf))
                 (file-in-directory-p (buffer-file-name buf) (magit-toplevel)))
        (with-current-buffer buf
          (vc-refresh-state)))))

  ;; [alias]
  ;;   update-master = !git fetch origin master:master
  ;;   update-main = !git fetch origin main:main
  (defun my/magit-update-master ()
    "Update git master or main branch."
    (interactive)
    (if (magit-toplevel)
        (let* ((branches (vc-git-branches))
               (main-p (member "main" branches))
               (current-branch (car branches))
               (on-master-p (member current-branch '("master" "main")))
               (command (concat "git " (if main-p "update-main" "update-master"))))
          (if on-master-p
              (vc-pull)
            (magit-shell-command-topdir command)))
      (message "Not a git repository")))

  ;; difftastic code copied from https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
  (defun my/magit--with-difftastic (buffer command)
    "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
    (let ((process-environment
           (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                         (number-to-string (frame-width)))
                 process-environment)))
      ;; Clear the result buffer (we might regenerate a diff, e.g., for
      ;; the current changes in our working directory).
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer))
      ;; Now spawn a process calling the git COMMAND.
      (make-process
       :name (buffer-name buffer)
       :buffer buffer
       :command command
       ;; Don't query for running processes when emacs is quit.
       :noquery t
       ;; Show the result buffer once the process has finished.
       :sentinel (lambda (proc event)
                   (when (eq (process-status proc) 'exit)
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-min))
                       (ansi-color-apply-on-region (point-min) (point-max))
                       (setq buffer-read-only t)
                       (view-mode)
                       (end-of-line)
                       ;; difftastic diffs are usually 2-column side-by-side,
                       ;; so ensure our window is wide enough.
                       (let ((width (current-column)))
                         (while (zerop (forward-line 1))
                           (end-of-line)
                           (setq width (max (current-column) width)))
                         ;; Add column size of fringes
                         (setq width (+ width
                                        (fringe-columns 'left)
                                        (fringe-columns 'right)))
                         (goto-char (point-min))
                         (pop-to-buffer
                          (current-buffer)
                          `(;; If the buffer is that wide that splitting the frame in
                            ;; two side-by-side windows would result in less than
                            ;; 80 columns left, ensure it's shown at the bottom.
                            ,(when (> 80 (- (frame-width) width))
                               #'display-buffer-at-bottom)
                            (window-width
                             . ,(min width (frame-width))))))))))))

  (defun my/magit-show-with-difftastic (rev)
    "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If REV is given, just use it.
            (when (boundp 'rev) rev)
            ;; If not invoked with prefix arg, try to guess the REV from
            ;; point's position.
            (and (not current-prefix-arg)
                 (or (magit-thing-at-point 'git-revision t)
                     (magit-branch-or-commit-at-point)))
            ;; Otherwise, query the user.
            (magit-read-branch-or-commit "Revision"))))
    (if (not rev)
        (error "No revision specified")
      (my/magit--with-difftastic
       (get-buffer-create (concat "*git show difftastic " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev))))

  (defun my/magit-diff-with-difftastic (arg)
    "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If RANGE is given, just use it.
            (when (boundp 'range) range)
            ;; If prefix arg is given, query the user.
            (and current-prefix-arg
                 (magit-diff-read-range-or-commit "Range"))
            ;; Otherwise, auto-guess based on position of point, e.g., based on
            ;; if we are in the Staged or Unstaged section.
            (pcase (magit-diff--dwim)
              ('unmerged (error "unmerged is not yet implemented"))
              ('unstaged nil)
              ('staged "--cached")
              (`(stash . ,value) (error "stash is not yet implemented"))
              (`(commit . ,value) (format "%s^..%s" value value))
              ((and range (pred stringp)) range)
              (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
    (let ((name (concat "*git diff difftastic"
                        (if arg (concat " " arg) "")
                        "*")))
      (my/magit--with-difftastic
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

  (require 'ansi-color)
  ;; https://tsdh.org/posts/2022-07-20-using-eldoc-with-magit-async.html
  ;; https://tsdh.org/posts/2021-06-21-using-eldoc-with-magit.html
  (defvar my/eldoc-git-show-stat--process nil)
  (defun my/eldoc-git-show-stat (callback commit)
    "Compute diffstat for COMMIT asynchronously, then call CALLBACK with it."
    ;; Kill the possibly still running old process and its buffer.
    (when (processp my/eldoc-git-show-stat--process)
      (let ((buf (process-buffer my/eldoc-git-show-stat--process)))
        (when (process-live-p my/eldoc-git-show-stat--process)
          (let (confirm-kill-processes)
            (kill-process my/eldoc-git-show-stat--process)))
        (when (buffer-live-p buf)
          (kill-buffer buf))))

    ;; Spawn a new "git show" process.
    (let* ((cmd (list "git" "--no-pager" "show"
                      "--no-color"
                      ;; Author Name <author@email.com>, <date-and-time>
                      "--format=format:%an <%ae>, %aD"
                      "--stat=80"
                      commit)))
      ;; An async eldoc-documentation-function must also return a non-nil,
      ;; non-string result if it's applicable for computing a documentation
      ;; string, so we set and return the new process here.
      (setq my/eldoc-git-show-stat--process
            (make-process
             :name "eldoc-git-show"
             :buffer (generate-new-buffer " *git-show-stat*")
             :noquery t
             :command cmd
             :sentinel (lambda (proc event)
                         (when (eq (process-status proc) 'exit)
                           (with-current-buffer (process-buffer proc)
                             (goto-char (point-min))
                             (put-text-property (point-min)
                                                (line-end-position)
                                                'face 'bold)
                             (funcall callback (buffer-string)))))))))

  (defvar my/magit-eldoc-last-commit nil)
  (defun my/magit-eldoc-for-commit (callback)
    (let ((commit (magit-commit-at-point)))
      (when (and commit
                 (not (equal commit my/magit-eldoc-last-commit)))
        (setq my/magit-eldoc-last-commit commit)
        (my/eldoc-git-show-stat callback commit))))

  (defun my/magit-eldoc-setup ()
    (add-hook 'eldoc-documentation-functions
              #'my/magit-eldoc-for-commit nil t))

  (add-hook 'magit-status-mode-hook #'my/magit-eldoc-setup)
  (add-hook 'magit-log-mode-hook #'my/magit-eldoc-setup)

  (eldoc-add-command 'magit-next-line)
  (eldoc-add-command 'magit-previous-line)

  ;; Based on https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
  (transient-define-prefix my/magit-extra-commands ()
    "Extra magit commands."
    ["Extra commands"
     ("u" "Set upstream" my/magit-set-upstream)
     ("r" "Refresh state (update modeline)" my/magit-refresh-state)
     ("m" "Update master/main" my/magit-update-master)
     ("d" "Difftastic Diff (dwim)" my/magit-diff-with-difftastic)
     ("s" "Difftastic Show" my/magit-show-with-difftastic)
     ("D" "Toggle magit-delta-mode" my/toggle-delta-mode)])
  (transient-append-suffix 'magit-dispatch "!"
    '("#" "Extra Magit Cmds" my/magit-extra-commands))
  (define-key magit-status-mode-map (kbd "#") #'my/magit-extra-commands)

  :custom
  (magit-diff-refine-hunk 'all)
  (magit-diff-paint-whitespace-lines 'all)
  (magit-diff-refine-ignore-whitespace nil)
  (magit-diff-highlight-trailing t))

(use-package magit-delta
  :after magit
  :demand t
  :config
  (defun my/toggle-delta-mode ()
    (interactive)
    (call-interactively #'magit-delta-mode)
    (magit-refresh)))

(use-package magit-todos)

(use-package forge
  :after magit)

(provide 'init-git)
;;; init-git.el ends here
