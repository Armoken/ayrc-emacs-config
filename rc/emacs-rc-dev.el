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

;; Remove excesses spaces in the string end, replace tabs by
;; spaces and align the intendation automaticaly before file saving
(defun untabify-buffer ()
	"Remove tabs from buffer."
	(interactive)
	(untabify (point-min) (point-max)))

(defun indent-buffer ()
	"Indent region."
	(interactive)
	(indent-region (point-min) (point-max)))

(defun cleanup-buffer-notabs ()
	"Perform a bunch of operations on the whitespace content of a buffer.
Remove tabs."
	(interactive)
	(indent-buffer)
	(untabify-buffer)
	(delete-trailing-whitespace)
	nil)

(defun cleanup-buffer-tabs ()
	"Perform a bunch of operations on the whitespace content of a buffer.
Dont remove tabs."
	(interactive)
	(indent-buffer)
	(delete-trailing-whitespace)
	nil)

(require 'cc-mode)
(c-add-style "microsoft"
			 '("stroustrup"
			   (c-offsets-alist
				(innamespace . -)
				(inline-open . 0)
				(inher-cont . c-lineup-multi-inher)
				(arglist-cont-nonempty . +)
				(template-args-cont . +))))
(setq c-default-style "microsoft" c-basic-offset 4)


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
(setq yas-snippet-dirs
      '("~/.emacs.d/other/yasnippet/yasmate/snippets"
        "~/.emacs.d/other/yasnippet/snippets"))
(yas-global-mode)

;; Project management with EDE
(require 'ede)
(global-ede-mode)

;; Flycheck (Check code errors)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-idle-change-delay 1)

;; Company-mode (Autocomplete)
;; Fuzzy complete
(require 'company)
(with-eval-after-load 'company (company-flx-mode +1))
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(setq company-idle-delay 0)
(add-hook 'after-init-hook 'global-company-mode)

(defun company-yasnippet-or-completion ()
	"Solve company yasnippet conflicts."
	(interactive)
	(let ((yas-fallback-behavior
			(apply 'company-complete-common nil)))
		(yas-expand)))

(add-hook 'company-mode-hook
		  (lambda ()
			  (setq c-default-style "microsoft"	c-basic-offset 4)
			  (substitute-key-definition
			   'company-complete-common
			   'company-yasnippet-or-completion
			   company-active-map)))

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

;; Language modes
(load "~/.emacs.d/rc/langs/emacs-rc-dev-short.el")
(load "~/.emacs.d/rc/langs/emacs-rc-dev-org.el")
(load "~/.emacs.d/rc/langs/emacs-rc-dev-nasm.el")

;; C-like modes
(load "~/.emacs.d/rc/langs/emacs-rc-dev-csharp.el")
(load "~/.emacs.d/rc/langs/emacs-rc-dev-cpp.el")

(load "~/.emacs.d/rc/langs/emacs-rc-dev-python.el")
(load "~/.emacs.d/rc/langs/emacs-rc-dev-latex.el")
(load "~/.emacs.d/rc/langs/emacs-rc-dev-lisp.el")
(load "~/.emacs.d/rc/langs/emacs-rc-dev-sql.el")

;; Web modes
(load "~/.emacs.d/rc/langs/emacs-rc-dev-html-css.el")
(load "~/.emacs.d/rc/langs/emacs-rc-dev-xml.el")
(load "~/.emacs.d/rc/langs/emacs-rc-dev-js.el")

(provide 'emacs-rc-dev)
;;; emacs-rc-dev.el ends here
