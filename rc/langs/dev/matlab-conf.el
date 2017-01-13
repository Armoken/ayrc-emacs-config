;;; matlab-conf.el --- Summary

;;; Commentary:
;; Settings only for Matlab

;;; Code:
(require 'matlab)
(require 'company)

(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-shell-command "matlab")

(defun my-matlab-mode-hook()
    (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t))
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)

(provide 'matlab-conf)
;;; matlab-conf.el ends here
