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
(add-hook 'LaTeX-mode-hook
		  '(lambda ()
			(setq-local company-backends
			 (append '((company-math-symbols-latex company-latex-commands))
			  company-backends))
			(company-auctex-init)
			(define-key LaTeX-mode-map (kbd "$")
			 'self-insert-command)))

(add-hook 'LaTeX-mode-hook
		  (lambda()
			  (add-hook 'write-contents-hooks
						'cleanup-buffer-notabs nil t)))

(provide 'emacs-rc-dev-latex)
;;; emacs-rc-dev-latex.el ends here
