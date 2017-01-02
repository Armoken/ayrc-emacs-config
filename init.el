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

(load "~/.emacs.d/rc/autoinstall-conf.el")
;; (my-auto-install-packages) ;; Uncomment on first start!

(mapc (lambda (path) (load path))
      (list "~/.emacs.d/rc/text-common-conf.el"
            "~/.emacs.d/rc/langs/langs-common-conf.el"

            "~/.emacs.d/rc/keybindings-conf.el"

            "~/.emacs.d/rc/ui/ui-common-conf.el"
            "~/.emacs.d/rc/ui/ui-helm-conf.el"
            "~/.emacs.d/rc/ui/ui-diminish-conf.el"

            "~/.emacs.d/rc/emms-conf.el"))

(provide 'init)
;;; init.el ends here
