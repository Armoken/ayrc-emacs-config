;;; emacs-rc-dev-asm --- Summary

;;; Commentary:
;; Settings only for asm

;;; Code:
(require 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))
(add-hook 'nasm-mode-hook
		  (lambda()
			  (add-hook 'write-contents-hooks
						'cleanup-buffer-tabs nil t)))

(provide 'emacs-rc-dev-asm)
;;; emacs-rc-dev-asm.el ends here
