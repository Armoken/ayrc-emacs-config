;;; cc-conf.el --- Summary

;;; Commentary:
;;; Settings only for CSS

;;; Code:
(require 'company)
(require 'hideshow)
(require 'web-mode)
(require 'emmet-mode)

(setq web-mode-css-indent-offset 4)

(defun my-css-mode-hook ()
    "Hooks for CSS mode."
    (emmet-mode)
    (hs-minor-mode)
    (add-to-list (make-local-variable 'company-backends)
                 '(company-css company-yasnippet)))
(provide 'css-conf)
;;; css-conf.el ends here
