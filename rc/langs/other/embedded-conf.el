;;; embedded-conf.el --- Summary

;;; Commentary:
;; Settings only for embedded development

;;; Code:

(require 'platformio-mode)

;; Automatic enable platformio mode when ini file exists
(defun my-platformio-hook ()
    (platformio-conditionally-enable))

(add-hook 'c-mode-hook 'my-platformio-hook)
(add-hook 'c++-mode-hook 'my-platformio-hook)


(provide 'embedded-conf)
;;; embedded-conf.el ends here
