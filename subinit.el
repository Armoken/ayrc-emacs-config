;;; subinit.el --- Summary  -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:
;; Disable scratch buffer
(setq initial-major-mode 'fundamental-mode)


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
(require 'package)
(setq package-user-dir          (expand-file-name "elpa" ayrc/path-to-non-config-files-dir)
      package-gnupghome-dir     (expand-file-name "gnupg" package-user-dir)

      ;; Without that line, (package-initialize) is executed twice
      ;; (once during evaluation of the init file, and another after
      ;; Emacs finishes reading the init file).
      package-enable-at-startup nil

      ;; Turn off warnings
      ad-redefinition-action    'accept

      package-archives          '(("gnu"          . "http://elpa.gnu.org/packages/")
                                  ("melpa"        . "https://melpa.org/packages/")
                                  ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'no-littering)
    (package-install 'diminish)
    (package-install 'delight))
(eval-when-compile
    (require 'use-package))
(require 'bind-key)


;; Keep clean config dir
(with-no-warnings
    (setq no-littering-etc-directory ayrc/path-to-session-cache-dir)
    (setq no-littering-var-directory ayrc/path-to-session-cache-dir))
(require 'no-littering)


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
          gc-cons-percentage          0.7)
    (setq gc-cons-threshold gcmh-low-cons-threshold)

    (defun ayrc/gcmh-startup-hook ()
        (setq gc-cons-threshold  gcmh-high-cons-threshold
              gc-cons-percentage 0.5)
        (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect))
    (add-hook 'emacs-startup-hook #'ayrc/gcmh-startup-hook))

(use-package org
    :defer t
    :defines (org-id-track-globally)
    :commands (org-babel-tangle-file)
    :config
    (setq org-id-track-globally                         nil
          org-babel-use-quick-and-dirty-noweb-expansion t))


;; Load use-conf
(defvar ayrc/user-conf-template-filename
    (ayrc/expand-config-path "./other/user-conf-template.org"))
(defvar ayrc/user-conf-filename
    (ayrc/expand-config-path "./user-conf.org"))
(let* ((path-to-selected-config  (if (file-exists-p ayrc/user-conf-filename)
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

;; Load main config
(let* ((config-name             "main.org")
       (path-to-config          (ayrc/expand-config-path config-name))
       (config-in-build-dir     (expand-file-name config-name ayrc/path-to-build-dir))

       (main-exported-filename  (concat (file-name-base config-name) ".el"))
       (path-to-main-file       (expand-file-name main-exported-filename ayrc/path-to-build-dir))

       (autoloadables-base-name (expand-file-name (concat (file-name-base config-name) "-loaddefs")
                                                  ayrc/path-to-build-dir))
       (path-to-autoloadables   (expand-file-name (concat autoloadables-base-name ".el")
                                                  ayrc/path-to-build-dir))
       (path-to-autoloads       (expand-file-name (concat autoloadables-base-name "-autoloads.el")
                                                  ayrc/path-to-build-dir)))
    (when (ayrc/is-processing-required path-to-config config-in-build-dir)
        (dolist (path-to-file (list config-in-build-dir
                                    path-to-main-file
                                    path-to-autoloadables
                                    path-to-autoloads))
            (when (file-exists-p path-to-file)
                (delete-file path-to-file)))

        ;; `ORG-BABEL-TANGLE-FILE' can't tangle file and save results
        ;; in separate directory. Therefore we need to copy file to
        ;; build directory.
        (copy-file path-to-config config-in-build-dir)

        ;; Tangle file
        (dolist (output-filename
                 (org-babel-tangle-file config-in-build-dir))
            (message "EXPORTED %s" output-filename))

        (require 'autoload)
        (let* ((generated-autoload-file path-to-autoloads)
               (section-text            nil))
            ;; Copy only section text
            (let ((autoloads-buffer (get-buffer-create "*autoloads-creation*"))
                  ;; (autoload-modified-buffers nil)
                  )
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
                (kill-buffer autoloads-buffer))

            ;; Byte compile autoloadables
            (byte-compile path-to-autoloadables)))

    (load-file path-to-autoloads)
    (ayrc/load-file path-to-main-file ayrc/path-to-build-dir))


;; Load custom.el
(setq custom-file (expand-file-name "custom.el"
                                    ayrc/path-to-session-configs-dir))
(if (file-exists-p custom-file)
        (ayrc/load-file custom-file
                        ayrc/path-to-build-dir))

(provide 'subinit)
;;; subinit.el ends here
