;;; emacs-rc-text --- Summary

;;; Commentary:
;;; Здесь находятся настройки для работы с текстом общие всех режимов

;;; Code:

;; Возможность удалить выделенный текст при вводе поверх
(delete-selection-mode t)

;; Переход на строчку - Alt-g номер строки перехода
(global-set-key (kbd "M-g") 'goto-line)

;; Удаление символа на C-h
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Подсветка строки
(global-hl-line-mode 1)

(require 'paren)
(custom-set-faces
 '(show-paren-match ((t (:background "purple4"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))))

(setq show-paren-delay 0) ;; отключить задержку
(setq show-paren-style 'expression) ;; выделить цветом выражения между {},[],()
(show-paren-mode) ;; включить выделение выражений между {},[],()



;; Undo tree
(global-undo-tree-mode 1)
(defadvice undo-tree-visualize (around undo-tree-split-side-by-side activate)
	"Split undo-tree side-by-side."
	(let ((split-height-threshold nil)
		  (split-width-threshold 0))
		ad-do-it))

(defun clear-undo-tree ()
	"Cleaning up undo-tree."
	(interactive)
	(setq buffer-undo-tree nil))

;; Автоматический перенос строк
(setq word-wrap t) ;; переносить по словам
(global-visual-line-mode t)

(defun cfg:backward-delete-tab-whitespace ()
	"Delete many spaces as mash in tab."
	(interactive)
	(let ((p (point)))
		(if (and (eq indent-tabs-mode nil)
				 (>= p tab-width)
				 (eq (% (current-column) tab-width) 0)
				 (string-match "^\\s-+$" (buffer-substring-no-properties (- p tab-width) p)))
			(delete-backward-char tab-width)
			(delete-backward-char 1))))
(global-set-key (kbd "<backspace>") 'cfg:backward-delete-tab-whitespace)

;; Spell check
(load "~/.emacs.d/rc/emacs-rc-spelling.el")

(provide 'emacs-rc-text)
;;; emacs-rc-text.el ends here
