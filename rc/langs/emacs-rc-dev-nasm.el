;;; emacs-rc-dev-nasm --- Summary

;;; Commentary:
;; Settings only for asm

;;; Code:
(require 'company)
(require 'nasm-mode)

(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))
(add-hook 'nasm-mode-hook
          (lambda()
              (add-hook 'write-contents-functions 'cleanup-buffer-tabs nil t)))

(provide 'emacs-rc-dev-nasm)
;;; emacs-rc-dev-nasm.el ends here
