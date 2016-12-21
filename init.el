;;; init.el --- Summary

;;; Commentary:
;; Init settings

;;; Code:

;; User nick and email
(setq user-full-name "Armoken")
(setq user-mail-address "Alex.Armoken@gmail.com")

;; ELPA - Package manager
(require 'package)
(setq package-archives
      '(
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(load "~/.emacs.d/rc/emacs-rc-autoinstall.el")
;; (my-auto-install-packages) ;; Uncomment on first start!

(load "~/.emacs.d/rc/emacs-rc-ui.el")
(load "~/.emacs.d/rc/emacs-rc-dev.el")
(load "~/.emacs.d/rc/emacs-rc-helm.el")
(load "~/.emacs.d/rc/emacs-rc-text.el")
(load "~/.emacs.d/rc/emacs-rc-emms.el")

(provide 'init.el)
;;; init.el ends here
