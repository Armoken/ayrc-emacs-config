;;; emacs-rc-dev-make --- Summary

;;; Commentary:
;; Settings for CMake and Makefile modes

;;; Code:
(require 'company)
(require 'cmake-mode)
(setq cmake-tab-width 4)
(add-hook 'cmake-mode-hook (lambda()
                               (setq company-backends '(company-cmake))
                               (add-hook 'write-contents-hooks
                                         'cleanup-buffer-notabs nil t)))
(add-hook 'makefile-mode-hook (lambda()
                                  (add-hook 'write-contents-hooks
                                            'cleanup-buffer-tabs nil t)))

(provide 'emacs-rc-dev-make)
;;; emacs-rc-dev-make.el ends here
