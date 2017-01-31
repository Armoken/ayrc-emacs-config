;;; ui-gdb-conf.el --- Summary

;;; Commentary:
;; Settings only for GDB

;;; Code:

(require 'company)

;; Disable company in gdb mode
(setq company-global-modes '(not gud-mode))

;; GDB layout
(setq gdb-many-windows t
      gdb-show-main t)

(provide 'ui-gdb-conf)
;;; ui-gdb-conf.el ends here
