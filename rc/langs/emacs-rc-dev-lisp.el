;;; emacs-rc-dev-lisp --- Summary

;;; Commentary:
;; Settings only for Lisp

;;; Code:
(require 'company)
(defun elisp-hook()
	(hs-minor-mode)
	(setq company-backends '(company-elisp
							 company-semantic
							 company-yasnippet
							 company-capf :with company-dabbrev)))
(add-hook 'emacs-lisp-mode-hook 'elisp-hook)

(define-key emacs-lisp-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(require 'helm)
(require 'slime)
(require 'slime-company)
(require 'slime-autoloads)
(setq slime-net-coding-system 'utf-8-unix)
(setq inferior-lisp-program  "/usr/bin/sbcl")
(slime-setup '(slime-fancy slime-asdf
			   slime-indentation slime-company))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))

;; Indents
(setq-default lisp-body-indent 4)
(setq lisp-indent-function 'common-lisp-indent-function)

(provide 'emacs-rc-dev-lisp)
;;; emacs-rc-dev-lisp.el ends here
