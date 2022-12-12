;;; subinit.el --- Summary  -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:
;; Disable scratch buffer
(setq initial-major-mode 'fundamental-mode)

;; Remove *scratch* buffer
(if (get-buffer "*scratch*")
    (kill-buffer "*scratch*"))

;; Set UTF-8 as default encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

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


;;; Setup package management system
(with-no-warnings
    (setq
        straight-use-symlinks                   t
        straight-cache-autoloads                t
        straight-enable-use-package-integration t
        straight-use-package-by-default         nil
        straight-check-for-modifications        (if (executable-find "watchexec")
                                                    '(watch-files)
                                                    '(find-at-startup))
        straight-base-dir                       ayrc/path-to-non-config-files-dir
        straight-profiles                       '((nil . "../../../freezed-pkgs-versions.el"))
        straight-built-in-pseudo-packages       '(emacs nadvice python)

        ;; Turn off warnings
        ad-redefinition-action                  'accept))

(defvar bootstrap-version)
(let ((bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" ayrc/path-to-non-config-files-dir))
         (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
                "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))


;; Bootstrap 'use-package'
(straight-use-package 'use-package)
(straight-use-package 'bind-key)
(straight-use-package 'diminish)
(straight-use-package 'delight)

(if t
    (eval-when-compile
        (require 'use-package))
    (progn
        (require 'use-package)
        (setq use-package-compute-statistics    t
            use-package-minimum-reported-time 0.001
            use-package-verbose               t)))
(require 'bind-key)


;; Keep clean config dir
(straight-use-package 'no-littering)
(with-no-warnings
    (setq no-littering-etc-directory ayrc/path-to-session-cache-dir)
    (setq no-littering-var-directory ayrc/path-to-session-cache-dir))
(require 'no-littering)


;;; Increase startup speed using GC tuning
(use-package gcmh
    :straight t
    :demand t
    :diminish gcmh-mode
    :init
    (gcmh-mode 1))

(use-package org
    :straight t
    :defer t
    :defines (org-id-track-globally)
    :commands (org-babel-tangle-file)
    :config
    (setq org-id-track-globally                         nil
        org-babel-use-quick-and-dirty-noweb-expansion t))


;; Load use-conf
(defvar ayrc/user-conf-template-filename (ayrc/expand-config-path "./user-conf-template.org"))
(defvar ayrc/user-conf-filename          (ayrc/expand-config-path "./user-conf.org"))
(defvar ayrc/user-conf-changed           nil)
(let* ((path-to-selected-config     (if (file-exists-p ayrc/user-conf-filename)
                                        ayrc/user-conf-filename
                                        ayrc/user-conf-template-filename))
          (selected-config-filename (file-name-nondirectory path-to-selected-config))
          (config-in-build-dir      (expand-file-name selected-config-filename
                                        ayrc/path-to-build-dir))

          (main-exported-filename   (concat (file-name-base selected-config-filename)
                                        ".el"))
          (path-to-main-file        (expand-file-name main-exported-filename
                                        ayrc/path-to-build-dir)))
    (when (ayrc/is-processing-required path-to-selected-config path-to-main-file)
        (setq ayrc/user-conf-changed t)

        (dolist (path-to-file (list config-in-build-dir
                                  path-to-main-file))
            (when (file-exists-p path-to-file)
                (delete-file path-to-file)))

        ;; `ORG-BABEL-TANGLE-FILE' can't tangle file and save results
        ;; in separate directory. Therefore we need to copy file to
        ;; build directory.
        (copy-file path-to-selected-config config-in-build-dir)

        ;; Tangle file
        (message "EXPORTED %s" (org-babel-tangle-file config-in-build-dir)))

    (ayrc/load-file path-to-main-file ayrc/path-to-build-dir))

;;;;; README.org loading ;;;;;
(eval-when-compile
    (require 'autoload))
(defvar ayrc/conf-name     "README.org")
(defvar ayrc/conf-filename (ayrc/expand-config-path ayrc/conf-name))
(defvar ayrc/conf-changed  nil)
(let* ((config-in-build-dir            (expand-file-name ayrc/conf-name ayrc/path-to-build-dir))

          (main-exported-filename         (concat (file-name-base ayrc/conf-name) ".el"))
          (path-to-main-file              (expand-file-name main-exported-filename ayrc/path-to-build-dir))

          (autoloadables-base-name        (expand-file-name (concat (file-name-base ayrc/conf-name) "-loaddefs")
                                              ayrc/path-to-build-dir))
          (path-to-autoloadables          (expand-file-name (concat autoloadables-base-name ".el")
                                              ayrc/path-to-build-dir))
          (path-to-compiled-autoloadables (expand-file-name (concat autoloadables-base-name ".elc")
                                              ayrc/path-to-build-dir))
          (path-to-autoloads              (expand-file-name (concat autoloadables-base-name "-autoloads.el")
                                              ayrc/path-to-build-dir))

          (is-autoloads-updated           nil))
    (when (ayrc/is-processing-required ayrc/conf-filename config-in-build-dir)
        (setq ayrc/conf-changed t)

        (dolist (path-to-file (list config-in-build-dir
                                  path-to-main-file
                                  path-to-autoloadables
                                  path-to-compiled-autoloadables
                                  path-to-autoloads))
            (when (file-exists-p path-to-file)
                (delete-file path-to-file)))

        ;; `ORG-BABEL-TANGLE-FILE' can't tangle file and save results
        ;; in separate directory. Therefore we need to copy file to
        ;; build directory.
        (copy-file ayrc/conf-filename config-in-build-dir)

        ;; Tangle README.org file
        (dolist (output-filename
                    (org-babel-tangle-file config-in-build-dir))
            (message "EXPORTED %s" output-filename))

        ;; Create autoloads files from autoloadables (README-loaddefs.el)
        (unless (featurep 'autoload)
            (require 'autoload))
        (if (version< emacs-version "28.0.0")
            (let* ((generated-autoload-file path-to-autoloads)
                      (section-text            nil))
                ;; Copy only section text
                (let ((autoloads-buffer (get-buffer-create "*autoloads-creation*")))
                    (autoload-generate-file-autoloads path-to-autoloadables autoloads-buffer)

                    (with-current-buffer autoloads-buffer
                        (goto-char (point-min))

                        (let ((section-start (search-forward generate-autoload-section-header))
                                 (section-end   (search-forward generate-autoload-section-trailer)))
                            (setq section-text (buffer-substring section-start
                                                   section-end)))

                        (set-buffer-modified-p nil))
                    (kill-buffer autoloads-buffer))

                ;; Use full autoload rubric with copied section text
                (let ((autoloads-buffer (get-buffer-create "*autoloads-creation*")))
                    (with-current-buffer autoloads-buffer
                        (insert (autoload-rubric generated-autoload-file
                                    "package"))

                        (goto-char (point-min))
                        (search-forward ";; Local Variables:")

                        (insert section-text)
                        (write-file generated-autoload-file))
                    (kill-buffer autoloads-buffer)))
            (make-directory-autoloads ayrc/path-to-build-dir path-to-autoloads))
        (setq is-autoloads-updated t))

    (when (not (version< emacs-version "28.0.0"))
       (add-to-list 'load-path ayrc/path-to-build-dir))
    (load-file path-to-autoloads)

    ;; Load and compile README.el
    (let ((use-package-always-demand is-autoloads-updated))
        (ayrc/load-file path-to-main-file ayrc/path-to-build-dir))

    ;; Compile autoloadables if it required
    (when is-autoloads-updated
        (ayrc/byte-compile-file path-to-autoloadables
            path-to-compiled-autoloadables)))

;;;;; custom.el loading ;;;;;
(setq custom-file (expand-file-name "custom.el"
                      ayrc/path-to-session-configs-dir))
;; Delete custom.el if it outdated
(when (or (ayrc/is-processing-required ayrc/conf-filename custom-file)
          (ayrc/is-processing-required ayrc/user-conf-filename custom-file))
    (delete-file custom-file))
;; Load custom.el
(if (file-exists-p custom-file)
    (ayrc/load-file custom-file ayrc/path-to-build-dir))


(provide 'subinit)
;;; subinit.el ends here
