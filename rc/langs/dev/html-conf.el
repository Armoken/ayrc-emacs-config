;;; html-conf.el --- Summary

;;; Commentary:
;; Settings only for HTML

;;; Code:
(require 'company)
(require 'hideshow)
(require 'web-mode)
(require 'emmet-mode)
(require 'company-web-html)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))

(setq emmet-move-cursor-between-quotes t)
(eval-after-load
 "emmet-mode"
 '(progn
   (define-key emmet-mode-keymap (kbd "C-j") nil)
   (define-key emmet-mode-keymap (kbd "C-i") 'emmet-expand-line)))


;; auto tag closing
;; 0=no auto-closing
;; 1=auto-close with </
;; 2=auto-close with > and </
(setq web-mode-auto-close-style 2)

(setq indent-tabs-mode nil)
(setq web-mode-indent-style 4)
(setq web-mode-style-padding 4)
(setq web-mode-enable-auto-closing t)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-enable-auto-pairing nil)
(setq web-mode-enable-current-element-highlight t)

(add-to-list 'hs-special-modes-alist
             (list 'web-mode
                   "<!--\\|<[^/>]*[^/]>"
                   "-->\\|</[^/>]*[^/]>"
                   "<!--"
                   'web-mode-forward-sexp
                   nil))

(defun my-web-mode-hook ()
    "Hooks for Web mode."
    (emmet-mode)
    (hs-minor-mode)
    (define-key web-mode-map (kbd "RET") 'newline)
    (define-key web-mode-map (kbd "C-j") 'newline)
    (define-key web-mode-map (kbd "C-c h") 'hs-toggle-hiding)
    (define-key web-mode-map (kbd "<tab>") (lambda () (interactive)
                                               (web-mode-indent-line)))
    (add-to-list (make-local-variable 'company-backends)
                 '(company-web-html company-yasnippet))
    (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t))
(add-hook 'web-mode-hook 'my-web-mode-hook)

(provide 'html-conf)
;;; html-conf.el ends here
