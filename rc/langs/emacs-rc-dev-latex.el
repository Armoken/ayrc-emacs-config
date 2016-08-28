;;; emacs-rc-dev-latex --- Summary

;;; Commentary:
;; Settings only for Latex

;;; Code:
(require 'tex-site)
(require 'preview)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(require 'company-auctex)
(company-auctex-init)
(add-hook 'LaTeX-mode-hook
		  '(lambda ()
			(define-key LaTeX-mode-map (kbd "$")
			 'self-insert-command)))

(provide 'emacs-rc-dev-latex)
;;; emacs-rc-dev-latex.el ends here
