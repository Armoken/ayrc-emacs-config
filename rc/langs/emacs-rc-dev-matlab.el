;;; emacs-rc-dev-matlab --- Summary

;;; Commentary:
;; Settings only for Matlab

;;; Code:
(require 'matlab)
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-shell-command "matlab")

(provide 'emacs-rc-dev-matlab)
;;; emacs-rc-dev-matlab.el ends here
