;;; js-conf.el --- Summary

;;; Commentary:
;; Settings only for Javascript

;;; Code:
;; Need dash-functional
(require 'tern)
(require 'company)
(require 'js2-mode)
(require 'json-mode)

(defun my-js-mode-hook()
    (tern-mode t)
    (add-to-list (make-local-variable 'company-backends)
                 '(company-tern company-yasnippet))
    (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t))
(add-hook 'js-mode-hook 'my-js-mode-hook)

(provide 'js-conf)
;;; js-conf.el ends here
