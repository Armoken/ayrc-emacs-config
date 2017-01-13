;;; python-conf.el --- Summary

;;; Commentary:
;; Settings only for Python

;;; Code:
(require 'elpy)
(require 'python)
(require 'company)
(require 'flycheck)

(elpy-enable)

(defun my-python-mode-hook()
    (hs-minor-mode)
    (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t)
    (define-key python-mode-map (kbd "C-c h") 'hs-toggle-hiding)
    (setq flycheck-checker-error-threshold 1000)
    (add-to-list (make-local-variable 'company-backends)
                 '(elpy-company-backend company-yasnippet)))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(setq elpy-rpc-backend "jedi")
(add-to-list 'python-shell-completion-native-disabled-interpreters "ipython3")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt --pprint")

(provide 'python-conf)
;;; python-conf.el ends here
