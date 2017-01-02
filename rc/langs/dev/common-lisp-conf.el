;;; common-lisp-conf.el --- Summary

;;; Commentary:
;; Settings only for Lisp

;;; Code:
(require 'helm)
(require 'slime)
(require 'company)
(require 'slime-company)
(require 'slime-autoloads)

(defun elisp-hook()
    (hs-minor-mode)
    (add-to-list (make-local-variable 'company-backends) '(company-elisp
                                                           company-yasnippet))
    (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t))

(add-hook 'emacs-lisp-mode-hook 'elisp-hook)

(define-key emacs-lisp-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(setq slime-net-coding-system 'utf-8-unix)
(setq inferior-lisp-program  "/usr/bin/sbcl")
(slime-setup '(slime-fancy slime-asdf
               slime-indentation slime-company))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))

;; Indents
(setq-default lisp-body-indent 4)
(setq lisp-indent-function 'common-lisp-indent-function)
(provide 'common-lisp-conf)
;;; common-lisp-conf.el ends here
