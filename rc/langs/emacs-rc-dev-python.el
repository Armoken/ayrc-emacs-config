;;; emacs-rc-dev-python --- Summary

;;; Commentary:
;; Settings only for Python

;;; Code:
(require 'elpy)
(require 'python)
(require 'company)

(elpy-enable)
(add-hook 'python-mode-hook
          (lambda ()
              (hs-minor-mode)
              (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t)
              (define-key python-mode-map (kbd "C-c h") 'hs-toggle-hiding)
              (add-to-list (make-local-variable 'company-backends)
                           '(elpy-company-backend company-yasnippet))))

(setq elpy-rpc-backend "jedi")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt --pprint")

(provide 'emacs-rc-dev-python)
;;; emacs-rc-dev-python.el ends here
