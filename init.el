;;; init.el --- Summary

;;; Commentary:
;; Init settings

;;; Code:

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

;; File that keep the following settings: user full name,
;; user mail address, font settings, theme settings,
;; modeline settings
(if (file-exists-p "~/.emacs.d/rc/user-conf.el")
    (load "~/.emacs.d/rc/user-conf.el"))

(mapc (lambda (path) (load path))
      (list
       "~/.emacs.d/rc/ui/ui-common-conf.el"
       "~/.emacs.d/rc/ui/ui-helm-conf.el"
       "~/.emacs.d/rc/ui/ui-diminish-conf.el"
       "~/.emacs.d/rc/ui/ui-gdb-conf.el"

       "~/.emacs.d/rc/text-common-conf.el"
       "~/.emacs.d/rc/langs/langs-common-conf.el"

       "~/.emacs.d/rc/keybindings-conf.el"

       "~/.emacs.d/rc/eshell-conf.el"
       "~/.emacs.d/rc/magit-conf.el"
       "~/.emacs.d/rc/tramp-conf.el"
       "~/.emacs.d/rc/projectile-conf.el"
       "~/.emacs.d/rc/grep-conf.el"
       ))

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (bison-mode company-cmake cpputils-cmake irony-eldoc sed-mode ag helm-ag ack projectile yaml-mode web-mode wcheck-mode use-package undo-tree swiper sublimity sr-speedbar sqlup-mode sqlplus spacemacs-theme spaceline smooth-scrolling smooth-scroll slime-company omnisharp nlinum-relative neotree nasm-mode modern-cpp-font-lock matlab-mode markdown-mode magit json-mode js2-mode help-fns+ helm-tramp helm-projectile helm-flycheck helm-emmet helm-dash flycheck-irony elpy editorconfig ecb dockerfile-mode company-ycmd company-web company-tern company-quickhelp company-math company-irony-c-headers company-irony company-flx company-auctex cmake-mode cmake-ide clang-format benchmark-init all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "purple4"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))))
