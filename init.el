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
(mapc 'org-babel-load-file
      (mapcar (lambda (path)
                (expand-file-name
                 path user-emacs-directory))
              (list "./rc/ui.org"
                    "./rc/text.org"
                    "./rc/utils.org"
                    "./rc/user-conf.org"
                    "./rc/keybindings.org"
                    "./rc/development.org"
                    "./rc/misc-interactive-functions.org"
                    "./rc/langs/lisp.org"
                    "./rc/langs/other.org"
                    "./rc/langs/python.org"
                    "./rc/langs/shell.org"
                    "./rc/langs/build.org"
                    "./rc/langs/tex.org"
                    "./rc/langs/markup.org")))

(provide 'init)
;;; init.el ends here
