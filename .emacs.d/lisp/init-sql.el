;;; init-sql.el --- LSP Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-feature sql
  :commands add-sql-connection
  :config
  ;; partially inspired by https://dev.to/viglioni/emacs-as-sql-client-with-lsp-143l (but currently only works for postgres)
  (defun add-sql-connection (name product port server user password database extra)
    (let ((driver (if (eq product 'postgres)
                      "postgresql"
                    (symbol-name product))))
      (when (not (boundp 'sql-connection-alist))
        (setq sql-connection-alist nil))
      (add-to-list 'sql-connection-alist (list name
                                               (list 'sql-product `(quote ,product))
                                               (list 'sql-port port)
                                               (list 'sql-server server)
                                               (list 'sql-user user)
                                               (list 'sql-password password)
                                               (list 'sql-database (format "%s://%s:%s@%s:%s/%s?%s" product user password server port database extra))))
      (when (not (boundp 'lsp-sqls-connections))
        (setq lsp-sqls-connections nil))
      (add-to-list 'lsp-sqls-connections (list (cons 'driver driver) (cons 'dataSourceName (format "host=%s port=%s user=%s password=%s dbname=%s sslmode=disable" server port user password database ))))))

  ;; https://www.emacswiki.org/emacs/SqlMode
  (defun my-sql-save-history-hook ()
    (let ((lval 'sql-input-ring-file-name)
          (rval 'sql-product))
      (if (symbol-value rval)
          (let ((filename
                 (concat "~/.emacs.d/sql/"
                         (symbol-name (symbol-value rval))
                         "-history.sql")))
            (set (make-local-variable lval) filename))
        (error
         (format "SQL history will not be saved because %s is nil"
                 (symbol-name rval))))))

  :hook (sql-interactive-mode . my-sql-save-history-hook))


(use-package sqlup-mode
  :diminish
  :custom (sqlup-blacklist '("id" "ids"))
  :hook
  (sql-mode . sqlup-mode)
  (sql-interactive-mode . sqlup-mode)
  :bind ("C-c c C-u" . sqlup-capitalize-keywords-in-region))

(use-package sql-indent
  :commands sqlind-minor-mode)

(use-package sqlformat
  :bind (:map sql-mode-map ("C-c C-f" . sqlformat)))

(provide 'init-sql)
;;; init-sql.el ends here
