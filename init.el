;;; init.el --- Summary

;;; Commentary:
;; Init settings


;;; Code:

(require 'package)

;; Without that line, (package-initialize) is executed twice
;; (once during evaluation of the init file, and another after
;; Emacs finishes reading the init file).
(setq package-enable-at-startup nil)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)


;; Bootstrap 'use-package'
(unless (or (package-installed-p 'use-package)
            (package-installed-p 'diminish)
            (package-installed-p 'bind-key))
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'bind-key))

(eval-when-compile
    (require 'use-package))
(require 'diminish) ;; Used to reduce size of the mode name in modeline
(require 'bind-key)

;; Loading orgmode plugin, that used for notes, plaining and literate programming
(require 'org)

(defun expand-config-path (path)
    "Expand passed path relative to the EMACS user directory.
`PATH' - passed path"
    (expand-file-name
     path user-emacs-directory))

;; Load example use-conf if
(defvar user-conf-template-filename "./other/user-conf-template.org")
(defvar user-conf-filename "./rc/user-conf.org")
(org-babel-load-file (if (file-exists-p (expand-config-path
                                         user-conf-filename))
                             (expand-config-path user-conf-filename)
                         (expand-config-path user-conf-template-filename)))

(mapc 'org-babel-load-file
      (mapcar 'expand-config-path
              (list
               ;; Core
               "./rc/core.org"
               "./rc/misc-functions.org"
               "./rc/keybindings.org"
               "./rc/ui.org"
               "./rc/text.org"
               "./rc/development.org"
               "./rc/orgmode.org"
               "./rc/clients.org"

               ;; Minumum
               "./rc/langs/lisp.org"
               "./rc/langs/shell.org"
               "./rc/langs/other.org"
               "./rc/langs/markup.org"
               "./rc/langs/build.org"

               ;; Optional
               "./rc/langs/cc.org"
               "./rc/langs/python.org"
               "./rc/langs/tex.org"
               ;; "./rc/langs/haskell.org"
               ;; "./rc/langs/vhdl.org"
               )))
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fill-column-indicator modern-cpp-font-lock cmake-ide ccls irony-eldoc flycheck-irony company-irony-c-headers company-irony irony clang-format cmake-font-lock cmake-mode markdown-preview-mode markdown-mode+ jinja2-mode flycheck-yamllint yaml-mode flycheck-demjsonlint dotenv-mode flycheck-plantuml plantuml-mode ini-mode systemd nginx-mode dockerfile-mode company-shell geiser elisp-slime-nav lisp-extra-font-lock rainbow-delimiters kubernetes docker org-cliplink org-present org-bullets dap-mode realgud helm-xref helm-lsp lsp-ui company-lsp lsp-mode helm-gtags ggtags expand-region editorconfig highlight-indentation aggressive-indent yasnippet-snippets yasnippet dsvn magit company-statistics company-quickhelp company-flx company helm-flycheck flycheck-popup-tip flycheck flymake-diagnostic-at-point helm-flymake helm-projectile projectile helm-rg ag smartparens ws-butler anzu adaptive-wrap smart-shift avy undo-tree hydra helm neotree exec-path-from-shell switch-buffer-functions spaceline-all-the-icons spaceline spacemacs-theme bind-key diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "purple4"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))))
