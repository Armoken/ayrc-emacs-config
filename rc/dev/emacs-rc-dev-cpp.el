;;; emacs-rc-dev-cpp --- Summary

;;; Commentary:
;; Settings only for C/C++

;;; Code:

;; CC Mode is a powerful package that provides modes for
;; editing C and C-like files
(require 'cc-mode)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c++-mode-common-hook 'hs-minor-mode)
(define-key c-mode-map (kbd "C-c h") 'hs-toggle-hiding)
(define-key c++-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(setq c-default-style "bsd" c-basic-offset 4)

(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(setq irony-server-install-prefix
	  "~/.emacs.d/servers/Irony")
(defun my-irony-mode-hook ()
	"Completition hook."
	(define-key irony-mode-map [remap completion-at-point]
		'irony-completion-at-point-async)
	(define-key irony-mode-map [remap complete-symbol]
		'irony-completion-at-point-async))
(add-hook 'my-irony-mode-hook 'irony-mode)

(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(setq company-backends (delete
						'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
(add-to-list 'company-backends 'company-irony)

;; (optional) adds CC special commands to
;; `company-begin-commands' in order to
;; trigger completion at interesting places, such as after
;; scope operator std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(eval-after-load 'flycheck
				 '(add-hook 'flycheck-mode-hook
				   #'flycheck-irony-setup))

(provide 'emacs-rc-dev-cpp)
;;; emacs-rc-dev-cpp.el ends here
