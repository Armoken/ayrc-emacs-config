;;; emacs-rc-dev-short --- Summary

;;; Commentary:
;; Settings for little modes like Assembler or Matlab

;;; Code:
;; Assembler
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . asm-mode))

;; Matlab
(require 'matlab)
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-shell-command "matlab")

(provide 'emacs-rc-dev-short)
;;; emacs-rc-dev-short.el ends here
