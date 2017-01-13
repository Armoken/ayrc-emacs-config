;;; nasm-conf.el --- Summary

;;; Commentary:
;; Settings only for Nasm

;;; Code:
(require 'company)
(require 'nasm-mode)

(add-to-list 'auto-mode-alist '("\\.nasm)$" . nasm-mode))

(defun my-nasm-mode-hook()
    (add-hook 'write-contents-functions 'cleanup-buffer-tabs nil t))
(add-hook 'nasm-mode-hook 'my-nasm-mode-hook)

(provide 'nasm-conf)
;;; nasm-conf.el ends here
