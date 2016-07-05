;;; emacs-rc-dev-lisp --- Summary

;;; Commentary:
;; Settings only for Lisp

;;; Code:
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(define-key emacs-lisp-mode-map
	(kbd "C-c h")'hs-toggle-hiding)

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
