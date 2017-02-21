;;; user-conf.el --- Summary

;;; Commentary:
;; Personal settings

;;; Code:
;; User nick and email
(setq user-full-name "Armoken")
(setq user-mail-address "Alex.Armoken@gmail.com")

;;; Font
;; Work when emacs running as server
(setq default-frame-alist '((font . "Hack-10")))
;; Not work when emacs running as server, but will work
;; if previous version bad
;; (setq default-frame-alist '((font . "Hack :: Regular: 10:: Cyrillic")))

;; Theme
(load-theme 'spacemacs-dark t)

(require 'spaceline-config)
;; Themes: arrow, arrow-fade, bar, box, brace,
;; butt, chamfer, contour, curve,
;; rounded, roundstub, slant, wave, zigzag, nil,
(setq powerline-default-separator 'contour)

(spaceline-emacs-theme)
(spaceline-toggle-buffer-size-off)
(setq powerline-height 20)

(provide 'user-conf.el)
;;; user-conf.el ends here
