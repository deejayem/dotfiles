;;; init-windows.el --- Window/Buffer Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-feature emacs
  :custom
  (switch-to-buffer-obey-display-actions t)
  (split-height-threshold nil)
  (split-width-threshold 200))

(use-feature ibuffer
  :bind
  ("C-x M-b" . ibuffer)
  (:map ibuffer-mode-map
        ("{" . ibuffer-backwards-next-marked)
        ("}" . ibuffer-forward-next-marked)
        ("[" . ibuffer-backward-filter-group)
        ("]" . ibuffer-forward-filter-group)
        ("$" . ibuffer-toggle-filter-group))
  :hook (ibuffer-mode . ibuffer-auto-mode))

(use-feature casual-ibuffer
  :after ibuffer
  :bind (:map ibuffer-mode-map
              ("C-o" . casual-ibuffer-tmenu)
              ("F" . casual-ibuffer-filter-tmenu)
              ("s" . casual-ibuffer-sortby-tmenu)))

(use-feature winner
  :defer 5
  :custom (winner-boring-buffers-regexp "\*Minibuf-[0-9]+")
  :config
  (winner-mode +1)
  (defvar winner-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<left>") #'winner-undo)
      (define-key map (kbd "<right>") #'winner-redo)
      map))
  (dolist (cmd '(winner-undo winner-redo))
    (put cmd 'repeat-map 'winner-repeat-map)))

(use-feature windmove
  :defer 5
  :config (windmove-default-keybindings)
  :bind (("C-c M-<up>" . windmove-delete-up)
         ("C-c M-<down>" . windmove-delete-down)
         ("C-c M-<left>" . windmove-delete-left)
         ("C-c M-<right>" . windmove-delete-right)))

(use-package buffer-move
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(use-feature uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package fullframe
  :defer 4
  :config
  (fullframe magit-status magit-mode-bury-buffer)
  (fullframe vc-annotate quit-window)
  (fullframe elpaca-fetch-all quit-window))

(use-package transpose-frame
  :bind (:map ctl-x-4-map ("t" . transpose-frame)))

(use-package ace-window
  :diminish
  :config
  (defun ace-window-always-dispatch (arg)
    "Call `ace-window' with `aw-dispatch-always' set to t, passing through `ARG'."
    (interactive "p")
    (let ((aw-dispatch-always t))
      (ace-window arg)))
  :bind
  ([remap other-window] . ace-window)
  ("C-x O" . ace-window-always-dispatch)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :custom-face
  (aw-leading-char-face
   ((t (:foreground "white" :background "red"
                    :weight bold :height 2.5 :box (:line-width 10 :color "red"))))))

(use-package popper
  :bind (("M-`" . popper-toggle)
         ("M-¬" . popper-cycle)
         ("C-`" . popper-toggle-type))
  :custom
  (popper-group-function #'popper-group-by-perspective)
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*HTTP Response\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     help-mode
     helpful-mode
     compilation-mode))
  :hook (elpaca-after-init . (lambda ()
                               (popper-mode +1)
                               (popper-echo-mode +1))))

(use-package frog-jump-buffer
  :config
  (defun frog-jump-buffer-filter-buffer-ring (buffer)
    "Check if a BUFFER is in current buffer ring."
    (let ((bfr-ring (buffer-ring-current-ring)))
      (when bfr-ring
        (let ((ring (buffer-ring-ring-ring bfr-ring)))
          (unless (dynaring-empty-p ring)
            (dynaring-contains-p ring (buffer-ring--parse-buffer buffer)))))))

  (defun frog-jump-buffer-filter-same-mode-in-persp (buffer)
    "Check if a BUFFER is the same as the current major mode and perspective."
    (let ((current-mode major-mode))
      (and
       (with-current-buffer buffer
         (eq major-mode current-mode))
       (persp-is-current-buffer buffer))))

  (defun frog-jump-buffer-filter-persp-buffer-ring-or-same-mode (buffer)
    "Check if BUFFER is in the buffer-ring, falling back to persp + major mode."
    (let* ((bfr-ring (buffer-ring-current-ring))
           (ring (buffer-ring-ring-ring bfr-ring)))
      (if (dynaring-empty-p ring)
          (frog-jump-buffer-filter-same-mode-in-persp buffer)
        (frog-jump-buffer-filter-buffer-ring buffer))))

  (set-face-background 'frog-menu-posframe-background-face "black")

  (setq frog-jump-buffer-include-current-buffer nil
        frog-jump-buffer-default-filter 'frog-jump-buffer-filter-persp-buffer-ring-or-same-mode
        frog-jump-buffer-use-default-filter-actions nil
        frog-jump-buffer-filter-actions '(("A" "[all]" frog-jump-buffer-filter-all)
                                          ("M" "[mode]" frog-jump-buffer-filter-same-mode-in-persp)
                                          ("F" "[files]" frog-jump-buffer-filter-file-buffers)
                                          ("R" "[recentf]" frog-jump-buffer-filter-recentf) ;; TODO filter/replace frog-jump-buffer-recentf-buffers
                                          ("B" "[ring]" frog-jump-buffer-filter-buffer-ring)
                                          ("P" "[project]" frog-jump-buffer-filter-same-project)
                                          ("S" "[similar]" frog-jump-buffer-filter-similar-name)))
  :bind
  ("C-c C-," . frog-jump-buffer)
  ("C-x 4 C-," . frog-jump-buffer-other-window))

(use-package buffer-ring
  :diminish
  :config
  (defun persp-buffer-ring-create-and-switch ()
    "Create and switch to the buffer-ring for the current perspective."
    ;; Creating a ring automatically switches to it
    (buffer-ring-torus--create-ring (persp-current-name)))

  (defun persp-buffer-ring-switch ()
    "Switch to the buffer-ring for the current perspective."
    (buffer-ring-torus-switch-to-ring (persp-current-name)))

  ;; This is mostly just a convenience, to stop buffer-ring from prompting for the ring to use
  (defun persp-buffer-ring-add-buffer ()
    "Add the current buffer to ring for the current perspective."
    (interactive)
    (let ((inhibit-message t))
      (buffer-ring-add (persp-current-name))))
  :hook
  (elpaca-after-init . buffer-ring-mode)
  (persp-created . persp-buffer-ring-create-and-switch)
  (persp-switch . persp-buffer-ring-switch)
  :bind
  (:map buffer-ring-mode-map
        ("C-c C-b a" . persp-buffer-ring-add-buffer)
        ("C-<" . buffer-ring-prev-buffer)
        ("C->" . buffer-ring-next-buffer)))

(use-package buffer-flip
  :custom (buffer-flip-skip-patterns '("^[*]"))
  :config
  (defun persp-buffer-flip-skip-buffer (orig &rest args)
    (or (apply orig args)
        (persp-buffer-filter (car args))))
  (advice-add 'buffer-flip-skip-buffer :around 'persp-buffer-flip-skip-buffer)
  :bind  (("C-c C-<left>" . buffer-flip)
          (:map buffer-flip-map
                ( "C-<left>" .   buffer-flip-forward)
                ( "C-<right>" . buffer-flip-backward)
                ( "C-g" . buffer-flip-abort))))

(use-package iflipb
  :config
  (defun iflipb-persp-buffer-list ()
    "Buffer list for iflipb."
    (seq-filter 'buffer-live-p (persp-current-buffers* t)))
  (defun iflibp-abort ()
    "Abort buffer flipping and return to the original buffer."
    (interactive)
    (iflipb-restore-buffers))
  (defun iflipb-kill-current-buffer ()
    "Same as `kill-current-buffer' but keep the iflipb buffer list state.

Modified from `iflipb-kill-buffer'."
    (interactive)
    (call-interactively #'kill-current-buffer)
    (if (iflipb-first-iflipb-buffer-switch-command)
        (setq last-command 'kill-current-buffer)
      (if (< iflipb-current-buffer-index (length (iflipb-interesting-buffers)))
          (iflipb-select-buffer iflipb-current-buffer-index)
        (iflipb-select-buffer (1- iflipb-current-buffer-index)))
      (setq last-command 'iflipb-kill-current-buffer)))
  :custom (iflipb-buffer-list-function 'iflipb-persp-buffer-list)
  :bind
  ("C-x k" . iflipb-kill-current-buffer)
  ("<f12>" . iflipb-previous-buffer)
  ("<f11>" . iflipb-next-buffer)
  ("<f10>" . iflibp-abort))

(provide 'init-windows)
;;; init-windows.el ends here
