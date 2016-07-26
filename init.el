;;; init.el --- Summary

;;; Commentary:
;; Init settings

;; About all installed packages:
;; assync - Asynchronous processing in Emacs

;; auto-complete - Auto Completion for GNU Emacs (Omnisharp)

;; cl-lib - Common Lisp extensions for Emacs
;; (Company, SLIME, coffe-mode, js2-mode)

;; dash - A modern list api for Emacs.  No cl require
;; (Flycheck, git)

;; dash-functional - Collection of useful combinators for
;; Emacs Lisp (company-tern)

;; epl - EPL provides a convenient high-level API for
;; various package.el versions, and aims to overcome its
;; most striking idiocies.

;; f - Modern API for working with files and directories
;; (Git)

;; find-file-in-project - Find files in a project quickly,
;; on any OS (Elpy)

;; json - JavaScript Object Notation parser/generator (Tern)
;; highlight-indentation -  Minor modes for highlighting
;; indentation (Elpy)

;; let-alist - Easily let-bind values of an assoc-list by
;; their names (Flycheck)

;; pkg-info - Information about packages (Flycheck)

;; popup - Visual Popup User Interface

;; pyvenv - Python virtual environment interface (Elpy)

;; s - The long lost Emacs string manipulation library.(Git)

;; seq - Sequence manipulation functions (Flycheck)

;; swiper - Gives you an overview as you search for a regex
;; (find-file-in-project (Elpy))

;; thingatpt - The library can determine and retrieve
;; different kinds of thing (symbol, list, S-expression
;; (sexp), function definition) at point.

;;; Code:

;; ELPA
;; Package manager
(require 'package)
(package-initialize)
(setq package-archives
	  '(("gnu" . "http://elpa.gnu.org/packages/")
		("marmalade" .
		 "https://marmalade-repo.org/packages/")
		("melpa" . "http://melpa.milkbox.net/packages/")))

;; UI
(load "~/.emacs.d/rc/emacs-rc-ui.el")
;; Text
(load "~/.emacs.d/rc/emacs-rc-text.el")
;; Development
(load "~/.emacs.d/rc/emacs-rc-dev.el")
;; Helm (So very cool for separate config)
;; (load "~/.emacs.d/rc/emacs-rc-helm.el")

(provide 'init.el)
;;; init.el ends here
