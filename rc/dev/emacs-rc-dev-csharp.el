;;; emacs-rc-dev-csharp --- Summary

;;; Commentary:
;; Settings only for C#

;;; Code:
(add-hook 'csharp-mode-common-hook 'hs-minor-mode)

(autoload 'csharp-mode
		  "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
	  (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(load "~/.emacs.d/other/csharp-hs-forward-sexp")
(add-hook 'csharp-mode-hook
		  (lambda ()
			  (hs-minor-mode 1)
			  (setq hs-isearch-open t)
			  (local-set-key "\C-c h"  'hs-toggle-hiding)))

(require 'omnisharp)
(require 'omnisharp-utils)
(require 'omnisharp-server-actions)
(require 'omnisharp-auto-complete-actions)
(setq omnisharp--curl-executable-path "/usr/bin/curl")
(setq omnisharp-server-executable-path "~/.emacs.d/servers/OmniSharp/OmniSharp/bin/Debug/OmniSharp.exe")
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-to-list 'company-backends 'company-omnisharp)

(defun defsln ()
	"Interactive function.
To start server with standard
sln file If sln isn't standard and you will execute
omnisharp-start-omnisharp-server, it will find
nearest sln file"
	(interactive)
	(omnisharp-start-omnisharp-server "~/.emacs.d/servers/OmniSharp/OmniSharp.sln")
	(message "Trying to start omnisharp server"))

(provide 'emacs-rc-dev-csharp)
;;; emacs-rc-dev-csharp.el ends here
