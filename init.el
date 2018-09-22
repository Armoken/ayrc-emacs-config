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
      (mapcar 'expand-config-path  (list "./rc/core.org"
                                         "./rc/ui.org"
                                         "./rc/text.org"
                                         "./rc/utils.org"
                                         "./rc/keybindings.org"
                                         "./rc/development.org"
                                         "./rc/misc-interactive-functions.org"
                                         "./rc/orgmode.org"
                                         "./rc/langs/markup.org"
                                         "./rc/langs/lisp.org"
                                         "./rc/langs/haskell.org"
                                         "./rc/langs/other.org"
                                         "./rc/langs/python.org"
                                         "./rc/langs/ruby.org"
                                         "./rc/langs/cc.org"
                                         "./rc/langs/go.org"
                                         "./rc/langs/shell.org"
                                         "./rc/langs/build.org"
                                         "./rc/langs/tex.org"
                                         "./rc/langs/js.org"
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
    (ccls hlinum dsvn lsp-ui realgud disaster helm-xref cmake-ide malinka eglot lsp-haskell lsp-html clean-aindent-mode dtrt-indent company-lsp lsp-python lsp-mode cquery company-c-headers geiser rainbow-delimiters smartparens racket-mode sphinx-doc markdown-edit-indirect markdown-mode markdown-mode+ markdown-preview-mode yaml-mode ws-butler web-mode use-package undo-tree systemd switch-buffer-functions spacemacs-theme spaceline-all-the-icons smart-shift rvm rubocop robe rinari python-mode pyimport pyenv-mode-auto py-yapf py-isort projectile-rails pip-requirements paredit org-present org-cliplink org-bullets nlinum nginx-mode neotree lisp-extra-font-lock kubernetes jinja2-mode irony-eldoc ini-mode helm-rtags helm-projectile helm-flycheck helm-ag go-gopath go-eldoc flycheck-yamllint flycheck-rtags flycheck-pos-tip flycheck-popup-tip flycheck-irony flycheck-haskell flycheck-demjsonlint expand-region elpy elisp-slime-nav editorconfig dockerfile-mode docker company-statistics company-shell company-rtags company-quickhelp company-irony-c-headers company-irony company-inf-ruby company-go company-ghc company-flx company-auctex cmake-font-lock clang-format bundler avy anzu aggressive-indent ag adaptive-wrap)))
 '(safe-local-variable-values
   (quote
    ((eval add-to-list
           (make-local-variable
            (quote auto-mode-alist))
           (quote
            ("\\.h\\'" . c++-mode)))
     (eval add-to-list
           (quote auto-mode-alist)
           (quote
            ("\\.h\\'" . c++-mode)))
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "purple4"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))))
