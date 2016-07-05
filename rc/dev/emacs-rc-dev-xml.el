;;; emacs-rc-dev-xml --- Summary

;;; Commentary:
;; Settings only for XML

;;; Code:
(require 'sgml-mode)
(require 'nxml-mode)
(add-hook 'nxml-mode-common-hook 'hs-minor-mode)
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)
(add-to-list 'hs-special-modes-alist
			 '(nxml-mode
			   "<!--\\|<[^/>]*[^/]>"
			   "-->\\|</[^/>]*[^/]>"

			   "<!--"
			   sgml-skip-tag-forward
			   nil))
(provide 'emacs-rc-dev-xml)
;;; emacs-rc-dev-xml.el ends here
