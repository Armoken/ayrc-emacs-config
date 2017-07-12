;;; langs-common-conf.el --- Summary

;;; Commentary:
;; Here you can find settings that common for all langs

;;; Code:

;; Indents (Some languages need custom settings)
(setq-default tab-width 4)
(setq-default pc-basic-offset 4)
(setq-default standart-indent 4)
(setq-default indent-tabs-mode t)

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


;; Flycheck (Check code errors)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-idle-change-delay 1)

;; Company-mode (Autocomplete)
(require 'company)
(with-eval-after-load 'company (company-flx-mode +1))
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(setq company-idle-delay 0.0)
(setq company-minimum-prefix-length 1)
(add-hook 'after-init-hook 'global-company-mode)

;; set default `company-backends'
(setq company-backends '((company-files          ; files & directory
                          company-keywords       ; keywords
                          company-capf)
                         (company-abbrev company-dabbrev)))

(require 'company-quickhelp)
(company-quickhelp-mode 1)
(eval-after-load 'company
                 '(define-key company-active-map (kbd "M-h")
                   #'company-quickhelp-manual-begin))


;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

;; Dev langs
(load "~/.emacs.d/rc/langs/dev/cc-conf.el")
(load "~/.emacs.d/rc/langs/dev/common-lisp-conf.el")
(load "~/.emacs.d/rc/langs/dev/csharp-conf.el")
(load "~/.emacs.d/rc/langs/dev/css-conf.el")
(load "~/.emacs.d/rc/langs/dev/elisp-conf.el")
(load "~/.emacs.d/rc/langs/dev/html-conf.el")
(load "~/.emacs.d/rc/langs/dev/js-conf.el")
(load "~/.emacs.d/rc/langs/dev/make-conf.el")
(load "~/.emacs.d/rc/langs/dev/matlab-conf.el")
(load "~/.emacs.d/rc/langs/dev/nasm-conf.el")
(load "~/.emacs.d/rc/langs/dev/haskell-conf.el")
(load "~/.emacs.d/rc/langs/dev/python-conf.el")
(load "~/.emacs.d/rc/langs/dev/sql-conf.el")

;; Other langs
(load "~/.emacs.d/rc/langs/other/org-conf.el")
(load "~/.emacs.d/rc/langs/other/latex-conf.el")
(load "~/.emacs.d/rc/langs/other/embedded-conf.el")

(provide 'langs-common-conf)
;;; langs-common-conf.el ends here
