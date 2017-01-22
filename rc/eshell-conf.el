;;; eshell-conf.el --- Summary

;;; Commentary:
;; Settings only for Eshell

;;; Code:
(require 'eshell)

(setq eshell-history-size 1024)

(setenv "PATH"
        (concat
         "/usr/local/bin:/usr/local/sbin:"
         (getenv "PATH")))

(setq eshell-scroll-to-bottom-on-input 'all
      eshell-prefer-lisp-functions nil)

(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)

(defun curr-dir-git-branch-string (pwd)
    "Return current git branch as a string.
Or the empty string if PWD is not in a git repo (or the
git command is not found)."
    (interactive)
    (when (and (eshell-search-path "git")
               (locate-dominating-file pwd ".git"))
        (let ((git-output (shell-command-to-string
                           (concat "cd " "\"$(pwd)\""
                                   " && git branch | grep '\\*' "
                                   "| sed -e 's/^\\* //'"))))
            (if (> (length git-output) 0)
                (concat " at git:(" (substring git-output 0 -1) ")")
                "(no branch)"))))

(defun pwd-replace-home (pwd)
    "Replace home in PWD with tilde (~) character."
    (interactive)
    (let* ((home (expand-file-name (getenv "HOME")))
           (home-len (length home)))
        (if (and
             (>= (length pwd) home-len)
             (equal home (substring pwd 0 home-len)))
            (concat "~" (substring pwd home-len))
            pwd)))

(defun pwd-shorten-dirs (pwd)
    "Shorten all directory names in PWD except the last two."
    (let ((p-lst (split-string pwd "/")))
        (if (> (length p-lst) 2)
            (concat
             (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                          (substring elm 0 1)))
                        (butlast p-lst 2)
                        "/")
             "/"
             (mapconcat (lambda (elm) elm)
                        (last p-lst 2)
                        "/"))
            pwd)))  ;; Otherwise, we just return the PWD

(defun split-directory-prompt (directory)
    "Break up the DIRECTORY into a “parent” and a “base”."
    (if (string-match-p ".*/.*" directory)
        (list (file-name-directory directory) (file-name-base directory))
        (list "" directory)))

(setq eshell-prompt-function
      (lambda ()
          (let* ((directory (split-directory-prompt (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))))
                 (parent (car directory))
                 (name (cadr directory))
                 ;; (branch (or (curr-dir-git-branch-string (eshell/pwd)) "")) ;; Uncomment to see current git brunch
                 (branch ""))

              (if (eq 'dark (frame-parameter nil 'background-mode))
                  (concat   ;; Prompt for Dark Themes
                   (propertize parent 'face `(:foreground "#8888FF"))
                   (propertize name   'face
                               `(:foreground "#8888FF" :weight bold))
                   (propertize branch 'face `(:foreground "green"))
                   (propertize " $"   'face `(:weight ultra-bold))
                   (propertize " "    'face `(:weight bold)))

                  (concat    ;; Prompt for Light Themes
                   (propertize parent 'face `(:foreground "blue"))
                   (propertize name   'face
                               `(:foreground "blue" :weight bold))
                   (propertize branch 'face `(:foreground "dark green"))
                   (propertize " $"   'face `(:weight ultra-bold))
                   (propertize " "    'face `(:weight bold)))))))

(setq eshell-highlight-prompt nil)

(defun eshell-here ()
    "Making little Shells whenever you need them.
Opens up a new shell in the directory associated with the
current buffer's file.  The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                       default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
        (split-window-vertically (- height))
        (other-window 1)
        (eshell "new")
        (rename-buffer (concat "*eshell: " name "*"))

        (insert (concat "ls"))
        (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun ha/eshell-quit-or-delete-char (arg)
    "Used to exiting from a shell or to delete char.
ARG is current cursos pos."
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp))
        (progn
            (eshell-life-is-too-much) ; Why not? (eshell/exit)
            (ignore-errors
             (delete-window)))
        (delete-forward-char arg)))

(add-hook 'eshell-mode-hook
          (lambda ()
              (define-key eshell-mode-map (kbd "C-d")
                  'ha/eshell-quit-or-delete-char)
              (local-set-key (kbd "M-R") 'eshell-list-history)
              (local-set-key (kbd "M-r")
                             (lambda ()
                                 (interactive)
                                 (insert
                                  (ido-completing-read
                                   "Eshell history: "
                                   (delete-dups
                                    (ring-elements eshell-history-ring))))))))

;; Sometimes you just need to change something about the current file you are
;; editing…like the permissions or even execute it. Hitting Command-1 will
;; prompt for a shell command string and then append the current file to it
;; and execute it.

(defun execute-command-on-file-buffer (cmd)
    "Execute command for cur file.
CMD is command for execution."
    (interactive "Command to execute: ")
    (let* ((file-name (buffer-file-name))
           (full-cmd (concat cmd " " file-name)))
        (shell-command full-cmd)))

(defun execute-command-on-file-directory (cmd)
    "Execute command for cur file directory.
CMD is command for execution."
    (interactive "Command to execute: ")
    (let* ((dir-name (file-name-directory (buffer-file-name)))
           (full-cmd (concat "cd " dir-name "; " cmd)))
        (shell-command full-cmd)))

(provide 'eshell-conf.el)
;;; eshell-conf.el ends here
