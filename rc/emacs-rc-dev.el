;;; emacs-rc-dev --- Summary

;;; Commentary:
;; Here you can find settings that common for all modes that relating with development

;;; Code:

;; Comment/uncomment code block
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Compile - F9
(global-set-key [(f9)] 'compile)

;; Indents (Some languages need custom settings)
(setq-default tab-width 4)
(setq-default pc-basic-offset 4)
(setq-default standart-indent 4)
(setq-default indent-tabs-mode t)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Remove excesses spaces in the string end, replace tabs by spaces and align the intendation automaticaly before file saving
(defun format-current-buffer()
	(indent-region (point-min) (point-max)))
(defun untabify-current-buffer()
	(if (not indent-tabs-mode)
		(untabify (point-min) (point-max)))
	nil)

(add-to-list 'write-file-functions 'format-current-buffer)
(add-to-list 'write-file-functions
			 'delete-trailing-whitespace)

;; Syntax highlight
(require 'font-lock)
(setq font-lock-maximum-decoration t)

;; Automatically pairs braces and quotes
(electric-pair-mode)

;; Semantic is a package that provides language-aware
;; editing commands based on source code parsers.
;; Parsing is a process of analyzing source code based
;; on programming language syntax.
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

;; Folding
(require 'hideshow)
(defvar hs-special-modes-alist
  (mapcar 'purecopy
		  '((c-mode "{" "}" "/[*/]" nil nil)
			(c++-mode "{" "}" "/[*/]" nil nil)
			(csharp-mode "{" "}" "/[*/]" nil nil)
			(java-mode "{" "}" "/[*/]" nil nil)
			(js-mode "{" "}" "/[*/]" nil))))

;; Yasnippet
(require 'yasnippet)
(yas-global-mode)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(provide 'init-yasnippet)

;; Project management with EDE
(require 'ede)
(global-ede-mode)

;; Flycheck (Check code errors)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-idle-change-delay 1)

;; Company-mode (Autocomplete)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(define-key company-active-map (kbd "TAB") 'company-complete-selection)

;; Language modes
(load "~/.emacs.d/rc/dev/emacs-rc-dev-short.el")
(load "~/.emacs.d/rc/dev/emacs-rc-dev-org.el")

(load "~/.emacs.d/rc/dev/emacs-rc-dev-csharp.el")
(load "~/.emacs.d/rc/dev/emacs-rc-dev-cpp.el")

(load "~/.emacs.d/rc/dev/emacs-rc-dev-python.el")
(load "~/.emacs.d/rc/dev/emacs-rc-dev-latex.el")
(load "~/.emacs.d/rc/dev/emacs-rc-dev-lisp.el")
(load "~/.emacs.d/rc/dev/emacs-rc-dev-sql.el")

;; Web modes
(load "~/.emacs.d/rc/dev/emacs-rc-dev-html-css.el")
(load "~/.emacs.d/rc/dev/emacs-rc-dev-xml.el")
(load "~/.emacs.d/rc/dev/emacs-rc-dev-js.el")

(provide 'emacs-rc-dev)
;;; emacs-rc-dev.el ends here
