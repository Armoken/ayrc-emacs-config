;;; emacs-rc-dev-cpp --- Summary

;;; Commentary:
;; Settings only for C/C++

;;; Code:
(require 'irony)
(require 'cc-mode)
(require 'company)
(require 'cmake-ide)
(require 'company-irony-c-headers)

;; Open headers for C in c-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
;; Open headers for C++ in c++-mode
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

(setq irony-server-install-prefix "~/.emacs.d/servers/Irony")
(add-hook 'irony-mode-hook
          (lambda ()
              (define-key irony-mode-map [remap completion-at-point]
                  'irony-completion-at-point-async)
              (define-key irony-mode-map [remap complete-symbol]
                  'irony-completion-at-point-async)
              (irony-cdb-autosetup-compile-options)

              ;; (optional) adds CC special commands to
              ;; `company-begin-commands' in order to
              ;; trigger completion at interesting places, such as after
              ;; scope operator std::|
              (company-irony-setup-begin-commands)))

(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook
                             #'flycheck-irony-setup))

(defun cc-hook()
    (setq c-basic-offset 4)
    (setq company-backends '(company-irony
                             company-irony-c-headers
                             company-semantic
                             company-yasnippet))
    (hs-minor-mode)
    (irony-mode))

(add-hook 'c-mode-hook
          (lambda ()
              (cc-hook)
              (define-key c-mode-map (kbd "C-c h") 'hs-toggle-hiding)))

(add-hook 'c++-mode-hook
          (lambda ()
              (cc-hook)
              (define-key c++-mode-map (kbd "C-c h") 'hs-toggle-hiding)))

(cmake-ide-setup)

(provide 'emacs-rc-dev-cpp)
;;; emacs-rc-dev-cpp.el ends here
