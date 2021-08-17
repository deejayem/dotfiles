;;; init-crux.el --- Crux Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package crux
  :config
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
  :bind
  ("C-^" . crux-top-join-line)
  ("C-<backspace>" . crux-kill-line-backwards)
  ([remap kill-whole-line] . crux-kill-whole-line)
  ("C-a" . crux-move-beginning-of-line)
  ;; TODO don't need all of these
  ("C-<return>" . crux-smart-open-line)
  ("S-<return>" . crux-smart-open-line)
  ([remap open-line] . crux-smart-open-line)
  ("C-S-<return>" . crux-smart-open-line-above)
  ("M-o" . crux-smart-open-line-above)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
  ("C-c F" . crux-recentf-find-file)
  ("C-x 4 s" . crux-swap-windows)
  ("C-x M-o" . crux-other-window-or-switch-buffer)
  ("C-c e" . crux-eval-and-replace)
  ("C-c M-k" . crux-kill-other-buffers)
  ("C-c C-r" . crux-rename-buffer-and-file)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c w" . crux-cleanup-buffer-or-region)
  ("C-c M-o" . crux-open-with)
  ("C-M-z" . curx-indent-defun)
  ("C-c C-u" . crux-view-url)
  ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
  ("C-c C-j" . crux-switch-to-previous-buffer)
  ("C-c x" . crux-reopen-as-root))

(provide 'init-crux)
;;; init-crux.el ends here
