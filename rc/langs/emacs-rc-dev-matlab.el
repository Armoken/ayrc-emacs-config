;;; emacs-rc-dev-matlab --- Summary

;;; Commentary:
;; Settings only for Matlab

;;; Code:
(require 'matlab)
(require 'company)

(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-shell-command "matlab")
(add-hook 'matlab-mode-hook
          (lambda()
              (set (make-local-variable 'company-backends) '(company-semantic))
              (add-hook 'write-contents-functions
                        'cleanup-buffer-notabs nil t)))

(provide 'emacs-rc-dev-matlab)
;;; emacs-rc-dev-matlab.el ends here
