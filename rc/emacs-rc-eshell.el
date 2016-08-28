;;; emacs-rc-eshell --- Summary

;;; Commentary:
;; Settings for eshell

;;; Code:

(require 'eshell)

(defun eshell-here ()
	"Open new shell in current directory.
The eshell is renamed to match that directory to make multiple eshell windows easier."
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

(defun eshell/x ()
	"Closes current eshell."
	(insert "exit")
	(eshell-send-input)
	(delete-window))

(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
				(define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

(provide 'emacs-rc-eshell)
;;; emacs-rc-eshell.el ends here
