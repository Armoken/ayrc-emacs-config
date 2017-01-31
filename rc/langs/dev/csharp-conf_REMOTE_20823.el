;;; csharp-conf.el --- Summary

;;; Commentary:
;; Settings only for C#

;;; Code:
(require 'company)
(require 'hideshow)
(require 'csharp-mode)
(require 'omnisharp)
(require 'omnisharp-utils)
(require 'omnisharp-server-actions)
(require 'omnisharp-auto-complete-actions)

(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

(setq omnisharp--curl-executable-path "/usr/bin/curl")
(setq omnisharp-server-executable-path
      "~/.emacs.d/servers/OmniSharp/OmniSharp/bin/Debug/OmniSharp.exe")

(defun defsln ()
    "Interactive function.
To start server with standard
sln file If sln isn't standard and you will execute
omnisharp-start-omnisharp-server, it will find
nearest sln file"
    (interactive)
    (omnisharp-start-omnisharp-server
     "~/.emacs.d/servers/OmniSharp/OmniSharp.sln")
    (message "Trying to start omnisharp server"))

(defun my-csharp-mode-hook()
    (hs-minor-mode)
    (setq hs-isearch-open t)
    (define-key csharp-mode-map (kbd "C-c h") 'hs-toggle-hiding)
    (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t)
    (add-to-list (make-local-variable 'company-backends)
                 '(company-omnisharp company-yasnippet)))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(provide 'csharp-conf)
;;; csharp-conf.el ends here
