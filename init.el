;;; init.el --- Summary  -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:
;;; UI settings that should be made as quickly as possible
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

;; Stop showing native compilation warnings
(setq
    native-comp-async-report-warnings-errors nil
    warning-minimum-level                    :error
    warning-minimum-log-level                :error)


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

(defun ayrc/byte-compile-file (path-to-src-file path-to-compiled-file)
    "Compile file.
PATH-TO-SRC-FILE - original file
PATH-TO-COMPILED-FILE - output file"
    (let ((byte-compile-dest-file-function (lambda (_) path-to-compiled-file)))
        (byte-compile-file path-to-src-file)))

(defun ayrc/load-file (path-to-file
                          path-to-build-dir
                          &optional compile-only)
    "Load Emacs Lisp source code in the PATH-TO-FILE.
Save byte-compiled version to PATH-TO-BUILD-DIR.  Its function used instead of
original `load-file' because of `load-file' doesn't compiles code.  Load file
if COMPILE-ONLY nil"
    (let* ((file-name             (file-name-nondirectory path-to-file))
              (base-file-name        (file-name-base file-name))
              (path-to-compiled-file (concat path-to-build-dir
                                         base-file-name
                                         ".elc")))
        (unless compile-only
            (load-file path-to-file))

        (when (ayrc/is-processing-required path-to-file
                  path-to-compiled-file)
            (ayrc/byte-compile-file path-to-file path-to-compiled-file))))


(defvar ayrc/path-to-non-config-files-dir
    (ayrc/expand-config-path "runtime-artifacts/"))
(make-directory ayrc/path-to-non-config-files-dir t)

(defvar ayrc/path-to-build-dir
    (expand-file-name "build/" ayrc/path-to-non-config-files-dir))
(make-directory ayrc/path-to-build-dir t)

(defvar ayrc/path-to-session-cache-dir
    (expand-file-name "session-caches/"
        ayrc/path-to-non-config-files-dir))
(make-directory ayrc/path-to-session-cache-dir t)

(defvar ayrc/path-to-session-configs-dir
    (expand-file-name "session-configs/"
        ayrc/path-to-non-config-files-dir))
(make-directory ayrc/path-to-session-configs-dir t)


;;; Load subinit.el
(defvar ayrc/path-to-subinit (ayrc/expand-config-path "subinit.el"))
(defvar ayrc/path-to-compiled-subinit (expand-file-name "subinit.elc"
                                          ayrc/path-to-build-dir))

(if (ayrc/is-processing-required ayrc/path-to-subinit
        ayrc/path-to-compiled-subinit)
    (progn
        (load-file ayrc/path-to-subinit)
        (ayrc/byte-compile-file ayrc/path-to-subinit
            ayrc/path-to-compiled-subinit))
    (load-file ayrc/path-to-compiled-subinit))


;;; Compile init.el
(defvar ayrc/path-to-init (ayrc/expand-config-path "init.el"))
(defvar ayrc/path-to-compiled-init (ayrc/expand-config-path "init.elc"))
(when (ayrc/is-processing-required ayrc/path-to-init
          ayrc/path-to-compiled-init)
    (byte-compile-file ayrc/path-to-init))

(provide 'init)
;;; init.el ends here
