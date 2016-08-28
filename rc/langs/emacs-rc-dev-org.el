;;; emacs-rc-dev-org --- Summary

;;; Commentary:
;; Settings only for org-mode

;;; Code:
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords
	  '((sequence "TODO" "SELECTED" "|" "DONE")))
(setq org-todo-keyword-faces
	  '(("SELECTED" . "red")))

(provide 'emacs-rc-dev-org)
;;; emacs-rc-dev-org.el ends here
