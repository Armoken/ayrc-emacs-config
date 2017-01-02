;;; elisp-conf.el --- Summary

;;; Commentary:
;; Settings only for Emacs Lisp

;;; Code:
(require 'company)
(require 'company-elisp)

(add-hook 'emacs-lisp-mode-hook
          (lambda()
              (hs-minor-mode)
              (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t)
              (define-key emacs-lisp-mode-map (kbd "C-c h") 'hs-toggle-hiding)
              (add-to-list (make-local-variable 'company-backends)
                           '(company-elisp company-yasnippet))))

(provide 'elisp-conf)
;;; elisp-conf.el ends here
