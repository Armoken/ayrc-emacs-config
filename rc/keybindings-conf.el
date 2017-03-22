;;; keybindings-conf.el --- Summary

;;; Commentary:
;; Global keybindings

;;; Code:

;; Text
;; Go to the line --- Alt-g transition line number
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "<backspace>") 'cfg:backward-delete-tab-whitespace)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Scroll
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))

;; Langs
;; Next error - F7
(global-set-key [f7] 'next-error)
;; Previous error - F8
(global-set-key [f8] 'previous-error)

;; Compile - F9
(global-set-key [(f9)] 'compile)
(global-set-key (kbd "C-c m") 'cmake-ide-compile)

(global-set-key (kbd "RET") 'newline-and-indent)


;; UI
(global-set-key (kbd "<f5>") 'sr-speedbar-toggle)
(global-set-key (kbd "<f6>") 'neotree-toggle)

;; Helm
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key helm-map (kbd "C-q") 'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(defun rename-current-buffer-file ()
    "Renames current buffer and file it is visiting."
    (interactive)
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
        (if (not (and filename (file-exists-p filename)))
            (error "Buffer '%s' is not visiting a file!" name)
            (let ((new-name (read-file-name "New name: " filename)))
                (if (get-buffer new-name)
                    (error "A buffer named '%s' already exists!" new-name)
                    (rename-file filename new-name 1)
                    (rename-buffer new-name)
                    (set-visited-file-name new-name)
                    (set-buffer-modified-p nil)
                    (message "File '%s' successfully renamed to '%s'"
                             name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(provide 'keybindings-conf.el)
;;; keybindings-conf.el ends here
