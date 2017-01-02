;;; js-conf.el --- Summary

;;; Commentary:
;; Settings only for Javascript

;;; Code:
;; Need dash-functional
(require 'tern)
(require 'company)
(require 'js2-mode)
(require 'json-mode)

(add-hook 'js-mode-hook
          (lambda ()
              (tern-mode t)
              (add-to-list (make-local-variable 'company-backends)
                           '(company-tern company-yasnippet))
              (add-hook 'write-contents-functions
                        'cleanup-buffer-notabs nil t)))

(provide 'js-conf)
;;; js-conf.el ends here
