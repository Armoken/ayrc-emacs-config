;;; emacs-rc-dev-nasm --- Summary

;;; Commentary:
;; Settings only for nasm

;;; Code:

(require 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))
(add-hook 'nasm-mode-hook
		  (lambda()
			  (add-hook 'write-contents-hooks
						'cleanup-buffer-tabs nil t)))

(provide 'emacs-rc-dev-nasm)
;;; emacs-rc-dev-nasm.el ends here
