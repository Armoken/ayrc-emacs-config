;;; init.el --- Summary

;;; Commentary:
;;; Code:
;; Settings that should be applied as quickly as possible to keep
;; intact your eyes

;; Setting background color from Spacemacs theme, to prevent blinking
(set-background-color "#292b2e")
(set-face-background 'mode-line "#292b2e")

(defun ayrc/remove-gui-elements (&optional frame)
    "Remove some GUI elelements.
It placed here, not in org file, to increase speed of removing them

FRAME: screen area that contains one or more Emacs windows"
    (menu-bar-mode     -1)
    (tooltip-mode      -1)
    (tool-bar-mode     -1)
    (scroll-bar-mode   -1))

(ayrc/remove-gui-elements)
(add-to-list 'after-make-frame-functions #'ayrc/remove-gui-elements)

;; Setup package management system
(require 'package)

;; Without that line, (package-initialize) is executed twice
;; (once during evaluation of the init file, and another after
;; Emacs finishes reading the init file).
(setq package-enable-at-startup nil)

(setq package-archives '(
                         ("gnu"          . "http://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ))
(package-initialize)


;; Bootstrap 'use-package'
(unless (or (package-installed-p 'use-package)
            (package-installed-p 'diminish)
            (package-installed-p 'delight)
            (package-installed-p 'bind-key))
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'delight)
    (package-install 'bind-key))

(eval-when-compile
    (require 'use-package))
(require 'diminish) ;; Used to reduce size of the mode name in modeline
(require 'bind-key)

;; Loading orgmode plugin, that used for notes, plaining and literate programming
(require 'org)

(defun ayrc/expand-config-path (path)
    "Expand passed path relative to the EMACS user directory.
`PATH' - passed path"
    (expand-file-name
     path user-emacs-directory))

;; Load example use-conf if
(defvar user-conf-template-filename "./other/user-conf-template.org")
(defvar user-conf-filename "./rc/user-conf.org")
(org-babel-load-file (if (file-exists-p (ayrc/expand-config-path
                                         user-conf-filename))
                             (ayrc/expand-config-path user-conf-filename)
                         (ayrc/expand-config-path user-conf-template-filename)))

(mapc 'org-babel-load-file
      (mapcar 'ayrc/expand-config-path
              (list
               ;; Core
               "./rc/core.org"
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

(setq custom-file (ayrc/expand-config-path "custom.el"))
(if (file-exists-p custom-file)
        (load custom-file))

(provide 'init)
;;; init.el ends here
