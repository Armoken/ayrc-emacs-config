;;; emacs-rc-dev-html-css --- Summary

;;; Commentary:
;; Settings only for HTML and CSS

;;; Code:
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(require 'company)
;; load company mode html backend
(require 'company-web-html)

(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(setq emmet-move-cursor-between-quotes t)
(eval-after-load "emmet-mode"
				 '(progn
				   (define-key emmet-mode-keymap (kbd "C-j") nil)
				   (define-key emmet-mode-keymap (kbd "C-i") 'emmet-expand-line)))

(defun my-web-mode-hook ()
	"Hooks for Web mode."
	(setq web-mode-style-padding 2)
	(setq web-mode-markup-indent-offset 4)
	(setq web-mode-css-indent-offset 4)
	(setq web-mode-code-indent-offset 4)

	;; auto tag closing
	;; 0=no auto-closing
	;; 1=auto-close with </
	;; 2=auto-close with > and </
	(setq web-mode-auto-close-style 1)

	(setq indent-tabs-mode nil)
	(setq web-mode-indent-style 4)
	(setq web-mode-enable-current-element-highlight t)
	(setq company-backends '(company-web-html)))
(add-hook 'web-mode-hook 'my-web-mode-hook)

(require 'hideshow)
(add-to-list 'hs-special-modes-alist
			 (list 'web-mode
				   "<!--\\|<[^/>]*[^/]>"
				   "-->\\|</[^/>]*[^/]>"
				   "<!--"
				   'web-mode-forward-sexp
				   nil))

(add-hook 'web-mode-hook 'hs-minor-mode)
(define-key web-mode-map (kbd "C-c h") 'hs-toggle-hiding)


(provide 'emacs-rc-dev-html-css)
;;; emacs-rc-dev-html-css.el ends here
