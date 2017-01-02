;;; nasm-conf.el --- Summary

;;; Commentary:
;; Settings only for Nasm

;;; Code:
(require 'company)
(require 'nasm-mode)

(add-to-list 'auto-mode-alist '("\\.nasm)$" . nasm-mode))
(add-hook 'nasm-mode-hook
          (lambda()
              (add-hook 'write-contents-functions 'cleanup-buffer-tabs nil t)))

(provide 'nasm-conf)
;;; nasm-conf.el ends here
