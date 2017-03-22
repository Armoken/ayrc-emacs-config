;;; python-conf.el --- Summary

;;; Commentary:
;; Settings only for Python

;;; Code:
(require 'elpy)
(require 'python)
(require 'company)
(require 'flycheck)

(elpy-enable)

(defcustom python-autopep8-path (executable-find "autopep8")
  "Autopep8 executable path."
  :group 'python
  :type 'string)

(defun python-autopep8 ()
    "Automatically formats Python code to conform to the PEP 8 style guide.
$ autopep8 --in-place --aggressive --aggressive <filename>"
    (interactive)
    (when (eq major-mode 'python-mode)
        (shell-command
         (format "%s --in-place --aggressive %s" python-autopep8-path
                 (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t)))

(defun my-python-mode-hook()
    (hs-minor-mode)
    (projectile-mode)
    (add-hook 'write-contents-functions 'cleanup-buffer-no-indent-notabs nil t)
    (define-key python-mode-map "C-c C-a" 'python-autopep8)
    (setq flycheck-checker-error-threshold 1000)
    (setq flycheck-flake8-maximum-line-length 120)
    (add-to-list (make-local-variable 'company-backends)
                 '(elpy-company-backend company-yasnippet)))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(setq elpy-rpc-backend "jedi")
(add-to-list 'python-shell-completion-native-disabled-interpreters "ipython3")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt --pprint")

(provide 'python-conf)
;;; python-conf.el ends here
