;;; init-crux.el --- Crux Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package crux
  :defer 5
  :commands crux-start-or-switch-to
  :config
  (crux-with-region-or-sexp-or-line sp-kill-region)
  (crux-with-region-or-buffer shell-command-on-region)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  :bind
  ("C-^" . crux-top-join-line)
  ("C-<backspace>" . crux-kill-line-backwards)
  ([remap kill-whole-line] . crux-kill-whole-line)
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
  ("C-c C-M-x" . crux-eval-and-replace)
  ("C-c M-k" . crux-kill-other-buffers)
  ("C-c C-r" . crux-rename-buffer-and-file)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c w" . crux-cleanup-buffer-or-region)
  ("C-c M-o" . crux-open-with)
  ("C-c C-u" . crux-view-url)
  ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
  ("C-c C-!" . crux-reopen-as-root))

(provide 'init-crux)
;;; init-crux.el ends here
