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
      (list "~/.emacs.d/rc/ui/ui-common-conf.el"
            "~/.emacs.d/rc/ui/ui-helm-conf.el"
            "~/.emacs.d/rc/ui/ui-diminish-conf.el"

            "~/.emacs.d/rc/text-common-conf.el"
            "~/.emacs.d/rc/langs/langs-common-conf.el"

            "~/.emacs.d/rc/keybindings-conf.el"

            "~/.emacs.d/rc/emms-conf.el"))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (sublimity web-mode wcheck-mode undo-tree swiper sr-speedbar sqlup-mode spacemacs-theme spaceline slime-company rtags omnisharp nasm-mode mpg123 matlab-mode markdown-mode json-mode js2-mode helm-projectile helm-flyspell helm-flycheck helm-firefox helm-emms helm-emmet helm-css-scss google-c-style google flycheck-irony f emms-state emms-player-mpv emms-mode-line-cycle emms-info-mediainfo elpy ecb diminish company-web company-tern company-quickhelp company-math company-irony-c-headers company-irony company-flx company-auctex cmake-mode cmake-ide clang-format))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "purple4"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))))
