;;; xml-conf.el --- Summary

;;; Commentary:
;; Settings only for XML

;;; Code:
(require 'company)
(require 'hideshow)
(require 'nxml-mode)

(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.axml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))

(add-to-list 'hs-special-modes-alist
             (list 'nxml-mode
                   "<!--\\|<[^/>]*[^/]>"
                   "-->\\|</[^/>]*[^/]>"
                   "<!--"
                   'nxml-forward-element
                   nil))

(add-hook 'nxml-mode-hook
          (lambda ()
              (add-hook 'write-contents-functions 'cleanup-buffer-tabs nil t)
              (hs-minor-mode)))
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(provide 'xml-conf)
;;; xml-conf.el ends here
