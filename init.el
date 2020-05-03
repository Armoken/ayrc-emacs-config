;;; init.el --- Summary

;;; Commentary:
;;; Code:
;; UI settings that should be made as quickly as possible
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
(setq use-package-compute-statistics t)

(require 'diminish) ;; Used to reduce size of the mode name in modeline
(require 'bind-key)

;; Loading orgmode plugin, that used for notes, plaining and literate programming
(require 'org)

(defun ayrc/expand-config-path (path)
    "Expand passed path relative to the EMACS user directory.
`PATH' - passed path"
    (expand-file-name
     path user-emacs-directory))

(defun ayrc/get-file-age (path-to-file)
    "Get file age."
    (float-time
     (time-subtract (current-time)
                    (nth 5 (or (file-attributes (file-truename path-to-file))
                               (file-attributes path-to-file))))))

(defun ayrc/org-babel-load-file (path-to-file)
    "Load Emacs Lisp source code blocks in the Org PATH-TO-FILE.
This function exports the source code using `org-babel-tangle',
compiles tangled code and then loads the resulting file
using `load-file'.
Its function used instead of original `org-babel-load-file' because of
 `org-babel-load-file' compiles code on every load, even if original
file doesn't changed."
    (let* ((base-name     (file-name-sans-extension path-to-file))
           (exported-file (concat base-name ".el"))
           (compiled-file (concat base-name ".elc"))
           (is-compiled   nil))

        (unless (and (file-exists-p exported-file)
                     (> (ayrc/get-file-age path-to-file)
                        (ayrc/get-file-age exported-file)))
            ;; Tangle-file traversal returns reversed list of tangled files
            ;; and we want to evaluate the first target.
            (setq exported-file
                  (car (last (org-babel-tangle-file path-to-file
                                                    exported-file
                                                    "emacs-lisp")))))

        (unless (and (file-exists-p compiled-file)
                     (> (ayrc/get-file-age path-to-file) (ayrc/get-file-age compiled-file)))
            (byte-compile-file exported-file)
            (setq is-compiled 't))

        (load-file exported-file)
        (message "%s %s"
                 (if is-compiled
                         (progn "Compiled and loaded")
                     (progn "Loaded"))
                 exported-file)))


(defun ayrc/load-file (path-to-file)
    "Load Emacs Lisp source code in the PATH-TO-FILE.
Its function used instead of original `load-file' because of
`load-file' doesn't compiles code."
    (let* ((base-file-name     (file-name-sans-extension path-to-file))
           (path-to-compiled-file (concat base-file-name ".elc")))
        (unless (and (file-exists-p path-to-compiled-file)
                     (> (ayrc/get-file-age path-to-file)
                        (ayrc/get-file-age path-to-compiled-file)))
            (byte-compile-file path-to-file)
            (message "%s %s" "Compiled" path-to-file))))

;; Byte-compile init.el
(ayrc/load-file (ayrc/expand-config-path "init.el"))

;; Load use-conf
(defvar user-conf-template-filename "./other/user-conf-template.org")
(defvar user-conf-filename "./user-conf.org")
(ayrc/org-babel-load-file (if (file-exists-p (ayrc/expand-config-path
                                              user-conf-filename))
                                  (ayrc/expand-config-path user-conf-filename)
                              (ayrc/expand-config-path user-conf-template-filename)))

;; Load main config
(ayrc/org-babel-load-file (ayrc/expand-config-path "./main.org"))

;; Load custom.el
(setq custom-file (ayrc/expand-config-path "custom.el"))
(if (file-exists-p custom-file)
        (ayrc/load-file custom-file))

(provide 'init)
;;; init.el ends here
