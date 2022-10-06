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

(use-package uniquify
  :ensure nil
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
  :bind (("<f5>"   . popper-toggle-latest)
         ("<f6>"   . popper-cycle)
         ("<f7>" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*HTTP Response\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(provide 'init-windows)
;;; init-windows.el ends here
