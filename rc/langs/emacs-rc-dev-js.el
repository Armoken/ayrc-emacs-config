;;; emacs-rc-dev-js --- Summary

;;; Commentary:
;; Settings only for Javascript

;;; Code:
(require 'company)
(require 'js2-mode)
(require 'json-mode)

;;(Need dash-functional)
(require 'tern)
(add-hook 'js-mode-hook (lambda ()
							(tern-mode t)
							(setq company-backends
								  '(company-tern))))

(provide 'emacs-rc-dev-js)
;;; emacs-rc-dev-js.el ends here
