;;; emacs-rc-dev-elisp --- Summary

;;; Commentary:
;; Settings only for Emacs Lisp

;;; Code:
(require 'company)

(add-hook 'emacs-lisp-mode-hook
          (lambda()
              (hs-minor-mode)
              (add-hook 'write-contents-hooks 'cleanup-buffer-notabs nil t)
              (define-key emacs-lisp-mode-map (kbd "C-c h") 'hs-toggle-hiding)
              (set (make-local-variable 'company-backends) '(company-elisp))))

(provide 'emacs-rc-dev-elisp)
;;; emacs-rc-dev-elisp.el ends here
