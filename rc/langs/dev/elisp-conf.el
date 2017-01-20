;;; elisp-conf.el --- Summary

;;; Commentary:
;; Settings only for Emacs Lisp

;;; Code:
(require 'company)
(require 'company-elisp)

(defun my-elisp-mode-hook()
    (hs-minor-mode)
    (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t)
    (define-key emacs-lisp-mode-map (kbd "C-c h") 'hs-toggle-hiding)
    (add-to-list (make-local-variable 'company-backends)
                 '(company-elisp company-yasnippet)))
(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(provide 'elisp-conf)
;;; elisp-conf.el ends here
