;; -*- lexical-binding: t; -*-
;;; init.el --- Summary
;;; Commentary:
;;; Code:
;;; UI settings that should be made as quickly as possible
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

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
;; (setq frame-inhibit-implied-resize t)


;;; Setup package management system
(require 'package)

;; Turn off warnings
(setq ad-redefinition-action 'accept)

;; Without that line, (package-initialize) is executed twice
;; (once during evaluation of the init file, and another after
;; Emacs finishes reading the init file).
(setq package-enable-at-startup nil)

(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)


;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'delight))

(eval-when-compile
    (require 'use-package))


;;; Increase startup speed using GC tuning
(use-package gcmh
    :ensure t
    :diminish gcmh-mode
    :init
    (setq gc-cons-percentage 0.6)
    (gcmh-mode 1)

    :config
    (setq gcmh-idle-delay          10
          gcmh-high-cons-threshold 16777216 ; 16mb
          gc-cons-percentage       0.1)
    (add-hook 'emacs-startup-hook
              (lambda ()
                  (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect))))

;; Unset file-name-handler-alist temporarily
;; Emacs consults this variable every time a file is read or library loaded,
;; or when certain functions in the file API are used (like expand-file-name
;; or file-truename).
;; Emacs does to check if a special handler is needed to read that file, but
;; none of them are (typically) necessary at startup
(defvar ayrc--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
              ;; Restore hacked values
              (setq file-name-handler-alist ayrc--file-name-handler-alist)))

;; Disable scratch buffer
(setq initial-major-mode 'fundamental-mode)


;;; Load other parts of configuration
(defun ayrc/expand-config-path (path)
    "Expand passed path relative to the EMACS user directory.
`PATH' - passed path"
    (expand-file-name
     path user-emacs-directory))

(defun ayrc/get-file-age (path-to-file)
    "Get age of file to which PATH-TO-FILE pointing."
    (float-time
     (time-subtract (current-time)
                    (nth 5 (or (file-attributes (file-truename path-to-file))
                               (file-attributes path-to-file))))))


(defun ayrc/is-processing-required (path-to-src-file path-to-result-file)
    "Check is need processing of source file for creating result file.
Return t if result file (PATH-TO-RESULT-FILE) not exists
or age of source file (PATH-TO-SRC-FILE) greater than age of
 result file (PATH-TO-RESULT-FILE)"
    (or (not (file-exists-p path-to-result-file))
        (> (ayrc/get-file-age path-to-result-file)
           (ayrc/get-file-age path-to-src-file))))

(defun ayrc/load-file (path-to-file &optional compile-only)
    "Load Emacs Lisp source code in the PATH-TO-FILE.
Its function used instead of original `load-file' because of
`load-file' doesn't compiles code.
Load file if COMPILE-ONLY nil"
    (let* ((base-file-name        (file-name-sans-extension path-to-file))
           (path-to-compiled-file (concat base-file-name ".elc")))
        (when (ayrc/is-processing-required path-to-file path-to-compiled-file)
            (byte-compile-file path-to-file)
            (message "Compiled %s" path-to-file))

        (unless compile-only
            (load-file path-to-file)
            (message "Loaded %s" path-to-file))))

(use-package org
    :defer t
    :commands (org-babel-tangle-file)
    :init
    (defun ayrc/org-babel-load-file (path-to-file &optional compile-only)
        "Load Emacs Lisp source code blocks in the Org PATH-TO-FILE.
This function exports the source code using `org-babel-tangle',
compiles tangled code and then loads the resulting file
using `load-file'.
Its function used instead of original `org-babel-load-file' because of
 `org-babel-load-file' compiles code on every load, even if original
file doesn't changed.
If COMPILE-ONLY passed than file will be only tangled and compiled"
        (let* ((base-name     (file-name-sans-extension path-to-file))
               (exported-file (concat base-name ".el")))
            (when (ayrc/is-processing-required path-to-file exported-file)
                ;; Tangle-file traversal returns reversed list of tangled files
                ;; and we want to evaluate the first target.
                (setq exported-file
                      (car (last (org-babel-tangle-file path-to-file
                                                        exported-file
                                                        "emacs-lisp")))))

            (ayrc/load-file exported-file compile-only))))


;; Byte-compile init.el
(ayrc/load-file (ayrc/expand-config-path "init.el") t)

;; Load use-conf
(defvar ayrc/user-conf-template-filename (ayrc/expand-config-path "./other/user-conf-template.org"))
(defvar ayrc/user-conf-filename (ayrc/expand-config-path "./user-conf.org"))
(ayrc/org-babel-load-file (if (file-exists-p ayrc/user-conf-filename)
                                  ayrc/user-conf-filename
                              ayrc/user-conf-template-filename))

;; Load main config
(ayrc/org-babel-load-file (ayrc/expand-config-path "./main.org"))

;; Load custom.el
(setq custom-file (ayrc/expand-config-path "custom.el"))
(if (file-exists-p custom-file)
        (ayrc/load-file custom-file))

(provide 'init)
;;; init.el ends here
