;;; emacs-rc-spelling --- Summary

;;; Commentary:
;; Spell check for russian and english

;;; Code:

(defvar lcl-var:spelling-ignore nil)
(defun lcl:spelling-add-to-dictionary (marked-text)
	(let* ((word (downcase (aref marked-text 0)))
		   (dict (if (string-match "[a-zA-Z]" word)
					 (message "en_US.dic")
					 (message "ru_RU.dic")))
		   (file (concat "~/.config/enchant/" dict)))
 		(when (and file (file-writable-p file))
			(with-temp-buffer
				(insert word) (newline)
				(append-to-file (point-min) (point-max) file)
				(message "Added word \"%s\" to the \"%s\" dictionary" word dict))
			(wcheck-mode 0)
			(wcheck-mode 1))))

(defun lcl:spelling-add-to-ignore (marked-text)
	(let ((word (aref marked-text 0)))
		(add-to-list 'lcl-var:spelling-ignore word)
		(message "Added word \"%s\" to the ignore list" word)
		(wcheck--hook-outline-view-change)))

(defun lcl:spelling-action-menu (marked-text)
	(append (wcheck-parser-ispell-suggestions)
			(list (cons "[Add to dictionary]" 'lcl:spelling-add-to-dictionary)
				  (cons "[Ignore]" 'lcl:spelling-add-to-ignore))))

(defun lcl:delete-list (delete-list list)
	(dolist (el delete-list)
		(setq list (remove el list)))
	list)

(defun lcl:spelling-parser-lines (&rest ignored)
	(lcl:delete-list lcl-var:spelling-ignore
					 (delete-dups
					  (split-string
					   (buffer-substring-no-properties (point-min) (point-max))
					   "\n+" t))))

(defun cfg:spelling ()
	(require 'wcheck-mode)
	(defun wcheck--choose-action-minibuffer (actions)
		(cdr
		 (assoc
		  (ido-completing-read "Choose " (mapcar #'car actions))
		  actions)))
	(setq-default
	 wcheck-language "All"
	 wcheck-language-data
	 '(("All"
		(program . "~/.emacs.d/other/spell_check_text.sh")
		(parser . lcl:spelling-parser-lines)
		(action-program . "~/.emacs.d/other/spell_check_word.sh")
		(action-parser . lcl:spelling-action-menu)
		(read-or-skip-faces
		 ((emacs-lisp-mode c-mode c++-mode csharp-mode python-mode)
		  read font-lock-comment-face)
		 (org-mode
		  skip org-block-begin-line org-block-end-line org-meta-line org-link)
		 (nil))
		))))
(cfg:spelling)


(provide 'emacs-rc-spelling)
;;; emacs-rc-spelling.el ends here
