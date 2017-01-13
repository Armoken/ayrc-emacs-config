;;; make-conf.el --- Summary

;;; Commentary:
;; Settings for CMake and Makefile modes

;;; Code:
(require 'company)
(require 'make-mode)
(require 'cmake-mode)

(setq cmake-tab-width 4)
(defun my-cmake-mode-hook()
    (set (make-local-variable 'company-backends)
         '(company-semantic company-cmake))
    (add-hook 'write-contents-functions
              'cleanup-buffer-notabs nil t))
(add-hook 'cmake-mode-hook 'my-cmake-mode-hook)

(defun my-make-hook ()
    "Func that set options for all make modes."
    (add-hook 'write-contents-functions
              '(lambda () (delete-trailing-whitespace) nil) nil t))
(mapc (lambda (hook) (add-hook hook 'my-make-hook))
      (list 'makefile-mode-hook
            'makefile-gmake-mode-hook
            'makefile-imake-mode-hook
            'makefile-bsdmake-mode-hook
            'makefile-automake-mode-hook
            'makefile-mode-hook))

(provide 'make-conf)
;;; make-conf.el ends here
