;;; js-conf.el --- Summary

;;; Commentary:
;; Settings only for Javascript

;;; Code:
;; Need dash-functional
(require 'tern)
(require 'company)
(require 'web-mode)
(require 'js2-mode)
(require 'json-mode)
(require 'company-tern)

(setq web-mode-code-indent-offset 4)
(add-to-list 'load-path "/path/to/tern/emacs/")

;; For the auto completion to work properly in your Node.js project, you need
;; to do some config. Open your project directory and create a file named
;; .tern-project with the content like this
;; {
;;   "plugins": {
;;     "node": {
;;     }
;;   }
;; }

(defun kill-tern-process ()
    "Function for killing Tern server.
Sometimes when you have just added .tern-project file or edit
the file but Tern does not auto reload, you need to manually kill
Tern server."
    (interactive)
    (delete-process "Tern"))

(defun my-js-mode-hook()
    (tern-mode)
    (add-to-list (make-local-variable 'company-backends)
                 '(company-tern company-yasnippet))
    (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t))
(add-hook 'js-mode-hook 'my-js-mode-hook)

(provide 'js-conf)
;;; js-conf.el ends here
