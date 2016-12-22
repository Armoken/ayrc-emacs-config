;;; emacs-rc-dev-make --- Summary

;;; Commentary:
;; Settings for CMake and Makefile modes

;;; Code:
(require 'company)
(require 'make-mode)
(require 'cmake-mode)

(setq cmake-tab-width 4)
(add-hook 'cmake-mode-hook
          (lambda()
              (set (make-local-variable 'company-backends)
                   '(company-semantic company-cmake))
              (add-hook 'write-contents-functions
                        'cleanup-buffer-notabs nil t)))

(defun my-make-hook ()
    "Func that set options for all make modes."
    (set (make-local-variable 'company-backends) '(company-semantic))
    (add-hook 'write-contents-functions
              '(lambda () (delete-trailing-whitespace) nil) nil t))

(mapc (lambda (hook) (add-hook hook 'my-make-hook))
      (list 'makefile-mode-hook
            'makefile-gmake-mode-hook
            'makefile-imake-mode-hook
            'makefile-bsdmake-mode-hook
            'makefile-automake-mode-hook
            'makefile-mode-hook))

(provide 'emacs-rc-dev-make)
;;; emacs-rc-dev-make.el ends here
