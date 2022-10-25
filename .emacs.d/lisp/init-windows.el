;;; init-windows.el --- Window/Buffer Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :custom
  (switch-to-buffer-obey-display-actions t)
  :bind
  ("C-x C-M-b" . ibuffer)
  :config
  ;; From EmacsWiki
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  (define-key ctl-x-4-map "t" 'toggle-window-split))

(use-package winner
  :defer 5
  :config
  (winner-mode +1)
  (defvar winner-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<left>") #'winner-undo)
      (define-key map (kbd "<right>") #'winner-redo)
      map))
  (dolist (cmd '(winner-undo winner-redo))
    (put cmd 'repeat-map 'winner-repeat-map)))

(use-package windmove
  :init (windmove-default-keybindings))

(use-package buffer-move
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(use-feature uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package fullframe
  :defer 8
  :config
  (fullframe magit-status magit-mode-bury-buffer)
  (fullframe vc-annotate quit-window))

(use-package ace-window
  :diminish
  :bind
  ([remap other-window] . ace-window)
  ("C-<tab>" . ace-window)
  :custom-face
  (aw-leading-char-face
   ((t (:foreground "white" :background "red"
                    :weight bold :height 2.5 :box (:line-width 10 :color "red"))))))

(use-package popper
  :bind (("M-`" . popper-toggle-latest)
         ("M-¬" . popper-cycle)
         ("C-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*HTTP Response\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     help-mode
     helpful-mode
     compilation-mode))
  :hook (emacs-startup . (lambda ()
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
        frog-jump-buffer-use-default-filter-actions t
        frog-jump-buffer-filter-actions '((("A" "[all]" frog-jump-buffer-filter-all)
                                           ("M" "[mode]" frog-jump-buffer-filter-same-mode-in-persp)
                                           ("F" "[files]" frog-jump-buffer-filter-file-buffers)
                                           ("R" "[recentf]" frog-jump-buffer-filter-recentf)
                                           ("B" "[Ring]" frog-jump-buffer-filter-buffer-ring)
                                           ("P" "[project]" frog-jump-buffer-filter-same-project)
                                           ("S" "[similar]" frog-jump-buffer-filter-similar-name))))
  :bind
  ("C-," . frog-jump-buffer)
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
  (emacs-startup . buffer-ring-mode)
  (persp-created . persp-buffer-ring-create-and-switch)
  (persp-switch . persp-buffer-ring-switch)
  :bind
  (:map buffer-ring-mode-map
        ("C-c C-b a" . persp-buffer-ring-add-buffer)
        ("C-<" . buffer-ring-prev-buffer)
        ("C->" . buffer-ring-next-buffer)))

(use-package cbm
  :config
  (defvar cbm-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<") #'cbm-cycle)
      map))
  (put 'cbm-cycle 'repeat-map 'cbm-repeat-map)
  :bind
  ("C-c C-b <" . cbm-cycle)
  ("C-c C-b C-b" . cbm-switch-buffer))

(use-package buffer-flip
  :bind  (("C-c C-<left>" . buffer-flip)
          (:map buffer-flip-map
                ( "C-<left>" .   buffer-flip-forward)
                ( "C-<right>" . buffer-flip-backward)
                ( "C-g" . buffer-flip-abort))))

(provide 'init-windows)
;;; init-windows.el ends here
