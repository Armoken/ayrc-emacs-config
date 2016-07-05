;;; emacs-rc-dev-html-css --- Summary

;;; Commentary:
;; Settings only for HTML and CSS

;;; Code:
(require 'web-mode)
(setq web-mode-indent-style 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-css-indent-offset 4)

(setq web-mode-auto-pairs t)
(setq web-mode-auto-close-style 2)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-tag-auto-close-style 2)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; Snippet and auto pair
(setq web-mode-extra-snippets
	  '(("erb" . (("name" . ("beg" . "end"))))))
(setq web-mode-extra-auto-pairs
	  '(("erb" . (("open" "close")))))
(setq web-mode-enable-current-element-highlight t)
;; load company mode html backend
(require 'company-web-html)
;; load company mode jade backend
(require 'company-web-jade)
;; load company mode slim backend
(require 'company-web-slim)

(require 'emmet-mode)
;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
;; enable Emmet's css abbreviation.
(add-hook 'css-mode-hook  'emmet-mode)
(setq emmet-move-cursor-between-quotes t)

(provide 'emacs-rc-dev-html-css)
;;; emacs-rc-dev-html-css.el ends here
