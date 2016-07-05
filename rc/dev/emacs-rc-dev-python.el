;;; emacs-rc-dev-python --- Summary

;;; Commentary:
;; Settings only for Python

;;; Code:
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook
		  (lambda() (define-key python-mode-map
						(kbd "C-c h") 'hs-toggle-hiding)))

(elpy-enable)
(setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "-i")

(provide 'emacs-rc-dev-python)
;;; emacs-rc-dev-python.el ends here
