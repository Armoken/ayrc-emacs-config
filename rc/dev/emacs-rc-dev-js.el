;;; emacs-rc-dev-js --- Summary

;;; Commentary:
;; Settings only for Javascript

;;; Code:
(require 'json-mode)
(require 'js2-mode)
(require 'coffee-mode)

;;(Need dash-functional)
(require 'tern)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-to-list 'company-backends 'company-tern)

(provide 'emacs-rc-dev-js)
;;; emacs-rc-dev-js.el ends here
