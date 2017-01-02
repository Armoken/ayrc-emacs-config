;;; matlab-conf.el --- Summary

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
              (add-hook 'write-contents-functions
                        'cleanup-buffer-notabs nil t)))

(provide 'matlab-conf)
;;; matlab-conf.el ends here
