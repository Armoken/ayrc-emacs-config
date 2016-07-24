;;; emacs-rc-dev-csharp --- Summary

;;; Commentary:
;; Settings only for C#

;;; Code:
(require 'hideshow)
(add-hook 'csharp-mode-common-hook
		  (lambda()
			  (hs-minor-mode)
			  (add-to-list 'write-file-functions
						   'untabify-current-buffer)))

(setq auto-mode-alist
	  (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(autoload 'csharp-mode
		  "csharp-mode" "Major mode for editing C# code." t)

(load "~/.emacs.d/other/csharp-hs-forward-sexp")
(add-hook 'csharp-mode-hook
		  (lambda ()
			  (local-set-key "\C-c h" 'hs-toggle-hiding)))

(require 'company)
(require 'omnisharp)
(require 'omnisharp-utils)
(require 'omnisharp-server-actions)
(require 'omnisharp-auto-complete-actions)
(setq omnisharp--curl-executable-path "/usr/bin/curl")
(setq omnisharp-server-executable-path "~/.emacs.d/servers/OmniSharp/OmniSharp/bin/Debug/OmniSharp.exe")
(add-hook 'csharp-mode-hook
		  (lambda()
			  (omnisharp-mode)
			  (setq company-backends '(company-omnisharp))))

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
