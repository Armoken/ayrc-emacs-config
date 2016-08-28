;;; emacs-rc-dev-cpp --- Summary

;;; Commentary:
;; Settings only for C/C++

;;; Code:

;; Open all headers in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

;; CC Mode is a powerful package that provides modes for
;; editing C and C-like files
(require 'cc-mode)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(define-key c-mode-map (kbd "C-c h") 'hs-toggle-hiding)
(add-hook 'c++-mode-common-hook 'hs-minor-mode)
(define-key c++-mode-map (kbd "C-c h") 'hs-toggle-hiding)


(require 'irony)
(setq irony-server-install-prefix "~/.emacs.d/servers/Irony")
(defun irony-hook()
	"Completition hook."
	(define-key irony-mode-map [remap completion-at-point]
		'irony-completion-at-point-async)
	(define-key irony-mode-map [remap complete-symbol]
		'irony-completion-at-point-async))
(add-hook 'irony-mode 'irony-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'company)
(require 'company-irony-c-headers)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

;; (optional) adds CC special commands to
;; `company-begin-commands' in order to
;; trigger completion at interesting places, such as after
;; scope operator std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook
							 #'flycheck-irony-setup))

(defun cc-hook()
	(setq c-default-style "microsoft"
		  c-basic-offset 4)

	(setq company-backends '(company-irony-c-headers
							 company-irony
							 company-semantic
							 company-yasnippet
							 company-capf :with company-dabbrev))
	(irony-mode))

(add-hook 'c-mode-hook 'cc-hook)
(add-hook 'c++-mode-hook 'cc-hook)

;; Cmake settings
(require 'cmake-mode)
(setq cmake-tab-width 4)

(defun cmake-hook()
	(setq company-backends '(company-cmake
							 company-semantic
							 company-yasnippet
							 company-capf :with company-dabbrev)))
(add-hook 'cmake-mode-hook 'cmake-hook)

(cmake-ide-setup)

(provide 'emacs-rc-dev-cpp)
;;; emacs-rc-dev-cpp.el ends here
