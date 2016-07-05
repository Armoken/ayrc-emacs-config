;;; emacs-rc-text --- Summary

;;; Commentary:
;;; Здесь находятся настройки для работы с текстом общие всех режимов

;;; Code:

;; Выделить выражение между {},[],()
(setq show-paren-delay 0) ;; отключить задержку
(setq show-paren-style 'expression) ;; выделить цветом выражения между {},[],()
(show-paren-mode 1) ;; включить выделение выражений между {},[],()

;; Возможность удалить выделенный текст при вводе поверх
(delete-selection-mode t)

;; Переход на строчку - Alt-g номер строки перехода
(global-set-key (kbd "M-g") 'goto-line)

;; Удаление символа на C-h
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Подсветка строки
(global-hl-line-mode 1)

;; Undo tree
(global-undo-tree-mode 1)
(defadvice undo-tree-visualize (around undo-tree-split-side-by-side activate)
	"Split undo-tree side-by-side"
	(let ((split-height-threshold nil)
		  (split-width-threshold 0))
		ad-do-it))

(defun clear-undo-tree ()
	"Cleaning up undo-tree"
	(interactive)
	(setq buffer-undo-tree nil))

;; Автоматический перенос строк
(setq word-wrap t) ;; переносить по словам
(global-visual-line-mode t)

;; Проверка правописания
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

(provide 'emacs-rc-text)
;;; emacs-rc-text.el ends here
