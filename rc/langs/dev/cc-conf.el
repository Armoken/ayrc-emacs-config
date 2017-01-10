;;; cc-conf.el --- Summary

;;; Commentary:
;; Settings only for C/C++

;;; Code:
(require 'irony)
(require 'cc-mode)
(require 'company)
(require 'flycheck)
(require 'cmake-ide)
(require 'clang-format)
(require 'google-c-style)
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

(defun cc-hook ()
    "Settings common for C and C++ modes."
    (irony-mode)
    (google-set-c-style)
    (setq c-basic-offset 4)
    (add-to-list (make-local-variable 'company-backends)
                 '(company-irony-c-headers company-irony company-yasnippet))
    (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t)
    (hs-minor-mode))

(add-hook 'c-mode-hook
          (lambda ()
              (cc-hook)
              (define-key c-mode-map (kbd "C-c h") 'hs-toggle-hiding)))

(add-hook 'c++-mode-hook
          (lambda ()
              (cc-hook)
              (setq flycheck-clang-language-standard "c++11")
              (setq irony-additional-clang-options '("-std=c++11"))
              (define-key c++-mode-map (kbd "C-c h") 'hs-toggle-hiding)))

(setq cmake-ide-flags-c++ (append '("-std=c++11")))
;; Compile with a keyboard shortcut
(global-set-key (kbd "C-c m") 'cmake-ide-compile)
(cmake-ide-setup)

(provide 'cc-conf)
;;; cc-conf.el ends here
