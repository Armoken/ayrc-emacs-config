;;; emms-conf.el --- Summary

;;; Commentary:
;; Settings only for Tramp mode

;;; Code:
(require 'tramp)

(setq tramp-default-method "sshx")
(defalias 'exit-tramp 'tramp-cleanup-all-buffers)
(define-key global-map (kbd "C-c s") 'helm-tramp)

;; If the shell of the server is zsh it is recommended to connect with bash.
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(provide 'tramp-conf.el)
;;; tramp-conf.el ends here
