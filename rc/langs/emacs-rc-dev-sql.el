;;; emacs-rc-dev-sql --- Summary

;;; Commentary:
;; Settings only for SQL

;;; Code:
(require 'sql)
(require 'company)

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

;; SQL Upcase
(require 'sqlup-mode)
;; Capitalize keywords in SQL mode
(add-hook 'sql-mode-hook
          (lambda ()
              (add-hook 'write-contents-functions 'cleanup-buffer-tabs nil t)
              (set (make-local-variable 'company-backends)
                   '(company-semantic))
              (sqlup-mode)))

;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
;; Set a global keyword to use sqlup on a region
;; (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)

(provide 'emacs-rc-dev-sql)
;;; emacs-rc-dev-sql.el ends here
