;;; docker-conf --- Summary

;;; Commentary:
;; Settings only for Dockerfile

;;; Code:
(require 'dockerfile-mode)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(provide 'docker-conf)

;;; docker-conf.el ends here
