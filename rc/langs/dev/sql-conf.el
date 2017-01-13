;;; sql-conf.el --- Summary

;;; Commentary:
;; Settings only for SQL

;;; Code:
(require 'sql)
(require 'company)
(require 'sqlup-mode) ;; Capitalize keywords in SQL mode

(setq sql-postgres-login-params
      '((user :default "noser")
        (database :default "testdb")
        (server :default "localhost")
        (port :default 5432)))

(add-hook 'sql-interactive-mode-hook
          (lambda () (toggle-truncate-lines t)))

(defun sql-save-history-hook ()
    "Save history."
    (let ((lval 'sql-input-ring-file-name)
          (rval 'sql-product))
        (if (symbol-value rval)
            (let ((filename
                    (concat "~/.emacs.d/sql/"
                            (symbol-name (symbol-value rval))
                            "-history.sql")))
                (set (make-local-variable lval) filename))
            (error
             (format "History will not be saved -  %s is nil"
                     (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'sql-save-history-hook)

(defun my-sql-mode-hook()
    (add-hook 'write-contents-functions 'cleanup-buffer-tabs nil t)
    (sqlup-mode))
(add-hook 'sql-mode-hook 'my-sql-mode-hook)

;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
;; Set a global keyword to use sqlup on a region
;; (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)

(provide 'sql-conf)
;;; sql-conf.el ends here
