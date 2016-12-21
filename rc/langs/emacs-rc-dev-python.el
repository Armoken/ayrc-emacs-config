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
              (add-hook 'write-contents-hooks 'cleanup-buffer-notabs nil t)
              (define-key python-mode-map (kbd "C-c h") 'hs-toggle-hiding)
              (set (make-local-variable 'company-backends)
                   '(company-mode/backend-with-yas 'elpy-company-backend))))

(setq elpy-rpc-backend "jedi")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt --pprint")

(provide 'emacs-rc-dev-python)
;;; emacs-rc-dev-python.el ends here
