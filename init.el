;; -*- lexical-binding: t; -*-
;;; init.el --- Summary
;;; Commentary:
;;; Code:
;;; UI settings that should be made as quickly as possible
(when (not (boundp 'ayrc/is-double-loading))
    (defun ayrc/remove-gui-elements (&optional _frame)
        "Remove some GUI elelements.
It placed here, not in org file, to increase speed of removing them

FRAME: screen area that contains one or more Emacs windows"
        (menu-bar-mode     -1)
        (tooltip-mode      -1)
        (tool-bar-mode     -1)
        (scroll-bar-mode   -1))
    (ayrc/remove-gui-elements)
    (add-to-list 'after-make-frame-functions #'ayrc/remove-gui-elements)

    ;; Resizing the Emacs frame can be a terribly expensive part of changing
    ;; the font. By inhibiting this, we halve startup times, particularly
    ;; when we use fonts that are larger than the system default (which would
    ;; resize the frame).
    (setq frame-inhibit-implied-resize t)


    ;; Hack from 'https://github.com/a13/fnhh' to increase startup speed
    (defvar ayrc/fnhh-handler-alist nil
        "A variable to store `file-name-handler-alist' initial value.")

    (defun ayrc/fnhh-restore ()
        "Restore variables and hooks."
        (when ayrc/fnhh-handler-alist
            (customize-set-variable 'file-name-handler-alist
                                    (copy-alist ayrc/fnhh-handler-alist)
                                    "Restored by FNHH")
            (setq ayrc/fnhh-handler-alist nil))
        (remove-hook 'emacs-startup-hook #'ayrc/fnhh-restore))

    (setq ayrc/fnhh-handler-alist file-name-handler-alist)
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook #'ayrc/fnhh-restore)


;;; Load other parts of configuration
    (defun ayrc/expand-config-path (path)
        "Expand passed path relative to the EMACS user directory.
`PATH' - passed path"
        (expand-file-name
         path user-emacs-directory))

    (defun ayrc/get-file-age (path-to-file)
        "Get age of file to which PATH-TO-FILE pointing."
        (float-time
         (time-subtract
          (current-time)
          (nth 5 (or (file-attributes (file-truename path-to-file))
                     (file-attributes path-to-file))))))

    (defun ayrc/is-source-file-changed (path-to-src path-to-result)
        "Check if the source (PATH-TO-SRC) of result (PATH-TO-RESULT) changed."
        (> (ayrc/get-file-age path-to-result)
           (ayrc/get-file-age path-to-src)))

    (defun ayrc/is-processing-required (path-to-src path-to-result)
        "Check is need processing of source file for creating result file.
Return t if result file (PATH-TO-RESULT) not exists
or age of source file (PATH-TO-SRC) greater than age of
 result file (PATH-TO-RESULT)"
        (or (not (file-exists-p path-to-result))
            (ayrc/is-source-file-changed path-to-src path-to-result)))

    (defun ayrc/load-file (path-to-file &optional compile-only)
        "Load Emacs Lisp source code in the PATH-TO-FILE.
Its function used instead of original `load-file' because of
`load-file' doesn't compiles code.
Load file if COMPILE-ONLY nil"
        (let* ((base-file-name        (file-name-sans-extension path-to-file))
               (path-to-compiled-file (concat base-file-name ".elc")))
            (when (ayrc/is-processing-required path-to-file
                                               path-to-compiled-file)
                (byte-compile-file path-to-file)
                (message "Compiled %s" path-to-file))

            (unless compile-only
                (load-file path-to-file)
                (message "Loaded %s" path-to-file))))

    (defvar ayrc/path-to-init (ayrc/expand-config-path "init.el"))
    (defvar ayrc/path-to-byte-compiled-init
        (ayrc/expand-config-path "init.elc"))
    (defvar ayrc/init-source-was-changed
        (ayrc/is-source-file-changed ayrc/path-to-init
                                     ayrc/path-to-byte-compiled-init))
    (defvar ayrc/is-init-exists
        (file-exists-p ayrc/path-to-byte-compiled-init))

    (defvar ayrc/is-double-loading nil))


;; Reload init.el when current running byte-compiled init.el older than source
(if (and ayrc/is-init-exists
         ayrc/init-source-was-changed
         (not ayrc/is-double-loading))
        (progn
            (message "Reloading of INIT.EL because of source was changed")
            (setq ayrc/is-double-loading t)
            (ayrc/load-file ayrc/path-to-init))

    ;;; Setup package management system
    (require 'package)

    ;; Turn off warnings
    (setq ad-redefinition-action 'accept)

    ;; Without that line, (package-initialize) is executed twice
    ;; (once during evaluation of the init file, and another after
    ;; Emacs finishes reading the init file).
    (setq package-enable-at-startup nil)

    (setq package-archives
          '(("gnu"          . "http://elpa.gnu.org/packages/")
            ("melpa"        . "https://melpa.org/packages/")
            ("melpa-stable" . "https://stable.melpa.org/packages/")))
    (package-initialize)


    ;; Bootstrap 'use-package'
    (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package)
        (package-install 'diminish)
        (package-install 'delight))

    ;; (eval-when-compile
    ;;     (require 'use-package))
    (require 'use-package)
    (setq use-package-compute-statistics t)


    ;;; Increase startup speed using GC tuning
    (use-package gcmh
        :ensure t
        :demand t
        :diminish gcmh-mode
        :config
        (setq gcmh-idle-delay             3600      ; 1 hour
              gcmh-low-cons-threshold     104857600 ; 100 MB
              gcmh-high-cons-threshold    209715200 ; 200 MB
              gcmh-verbose                nil

              garbage-collection-messages t
              gc-cons-percentage          0.5)
        (setq gc-cons-threshold gcmh-low-cons-threshold)

        (defun ayrc/gcmh-startup-hook ()
            (setq gc-cons-threshold  gcmh-high-cons-threshold
                  gc-cons-percentage 0.1)
            (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect))
        (add-hook 'emacs-startup-hook #'ayrc/gcmh-startup-hook))


    ;; Unset file-name-handler-alist temporarily
    ;; Emacs consults this variable every time a file is read or library
    ;; loaded, or when certain functions in the file API are used (like
    ;; expand-file-name or file-truename).
    ;; Emacs does to check if a special handler is needed to read that file,
    ;; but none of them are (typically) necessary at startup
    (defvar ayrc--file-name-handler-alist file-name-handler-alist)
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                  ;; Restore hacked values
                  (setq file-name-handler-alist
                        ayrc--file-name-handler-alist)))

    ;; Disable scratch buffer
    (setq initial-major-mode 'fundamental-mode)


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
                    (when (file-exists-p exported-file)
                        (delete-file exported-file))

                    ;; Tangle-file traversal returns reversed list of tangled
                    ;; files and we want to evaluate the first target.
                    (setq exported-file
                          (car (last (org-babel-tangle-file path-to-file
                                                            exported-file
                                                            "emacs-lisp")))))

                (ayrc/load-file exported-file compile-only)))
        :config
        (setq org-babel-use-quick-and-dirty-noweb-expansion t))


    (when (or (not ayrc/init-source-was-changed)
              ayrc/init-source-was-changed)
        (byte-compile-file ayrc/path-to-init))

    ;; Load use-conf
    (defvar ayrc/user-conf-template-filename
        (ayrc/expand-config-path "./other/user-conf-template.org"))
    (defvar ayrc/user-conf-filename
        (ayrc/expand-config-path "./user-conf.org"))
    (ayrc/org-babel-load-file (if (file-exists-p ayrc/user-conf-filename)
                                      ayrc/user-conf-filename
                                  ayrc/user-conf-template-filename))

    ;; Load main config
    (ayrc/org-babel-load-file (ayrc/expand-config-path "./main.org"))

    ;; Load custom.el
    (setq custom-file (ayrc/expand-config-path "custom.el"))
    (if (file-exists-p custom-file)
            (ayrc/load-file custom-file)))

(provide 'init)
;;; init.el ends here
