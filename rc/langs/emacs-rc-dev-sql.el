;;; emacs-rc-dev-sql --- Summary

;;; Commentary:
;; Settings only for SQL

;;; Code:
(require 'sql)

(setq sql-postgres-login-params
	  '((user :default "noser")
		(database :default "testdb")
		(server :default "localhost")
		(port :default 5432)))

(add-hook 'sql-interactive-mode-hook
		  (lambda ()
			  (toggle-truncate-lines t)))


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

(add-hook 'sql-interactive-mode-hook
		  'sql-save-history-hook)

;; SQL Upcase
(when (require 'sql-upcase nil :noerror)
	(add-hook 'sql-mode-hook 'sql-upcase-mode)
	(add-hook 'sql-interactive-mode-hook 'sql-upcase-mode))
(setq sql-upcase-mixed-case t)

(provide 'emacs-rc-dev-sql)
;;; emacs-rc-dev-sql.el ends here
