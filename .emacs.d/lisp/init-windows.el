;;; init-windows.el --- Window/Buffer Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :bind
  ("C-x O" . (lambda ()
               (interactive)
               (other-window -1)))
  ("C-x C-M-b" . ibuffer)
  :config
  (winner-mode +1)
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
  :after (magit dashboard)
  :config
  (fullframe magit-status magit-mode-quit-window)
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

(smartrep-define-key global-map
    "C-x" '(("}" . enlarge-window-horizontally)
            ("{" . shrink-window-horizontally)
            ("^" . enlarge-window)
            ("-" . shrink-window-if-larger-than-buffer)))

(provide 'init-windows)
;;; init-windows.el ends here
