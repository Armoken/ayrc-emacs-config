;;; emacs-rc-emms --- Summary

;;; Commentary:
;; Settings for emms

;;; Code:

(require 'emms)
(require 'emms-setup)

(add-to-list 'load-path "~/.emacs.d/emms/")
(emms-standard)
(emms-default-players)

(provide 'emacs-rc-emms)
;;; emacs-rc-emms.el ends here
