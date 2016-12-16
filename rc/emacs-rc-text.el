;;; emacs-rc-text --- Summary

;;; Commentary:
;;; Text settings common for all modes

;;; Code:

;; From Emacs manual: If you enable Delete Selection mode, a minor mode,
;; then inserting text while the mark is active causes the selected text
;; to be deleted first. This also deactivates the mark. Many graphical
;; applications follow this convention, but Emacs does not.
(delete-selection-mode t)

;; Go to the line --- Alt-g transition line number
(global-set-key (kbd "M-g") 'goto-line)

;; Backspace on C-h
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Current line hightlight
(global-hl-line-mode 1)

;; Common clipboard with X Server
(setq x-select-enable-clipboard t)

;; Show-paren-mode settings
(require 'paren)
(custom-set-faces
 '(show-paren-match ((t (:background "purple4"))))
 '(show-paren-mismatch
   ((((class color)) (:background "red" :foreground "white")))))

(show-paren-mode t) ;; highlight expressions between {},[],()
(setq show-paren-delay 0) ;; disable delay
(setq show-paren-style 'expression) ;; hightlight type
(show-paren-mode) ;; enable global minor mode

;; Line numbers
(require 'linum)
(line-number-mode t) ;; show line number in mode-line
(global-linum-mode t) ;; show line number in all buffers
(column-number-mode t) ;; show column number in mode-line
(setq linum-format " %d ") ;; format of line numbers

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(defadvice undo-tree-visualize
	(around undo-tree-split-side-by-side activate)
	"Split undo-tree side-by-side."
	(let ((split-height-threshold nil)
		  (split-width-threshold 0))
		ad-do-it))

(defun clear-undo-tree ()
	"Cleaning up undo-tree."
	(interactive)
	(setq buffer-undo-tree nil))

;; Automatic line wrapping
(setq word-wrap t) ;; wrap by words
(global-visual-line-mode t)

(defun cfg:backward-delete-tab-whitespace ()
	"Delete many spaces as mash in tab."
	(interactive)
	(let ((p (point)))
		(if
		 (and
		  (eq indent-tabs-mode nil)
		  (>= p tab-width)
		  (eq (% (current-column) tab-width) 0)
		  (string-match "^\\s-+$"
						(buffer-substring-no-properties (- p tab-width) p)))
		 (delete-char (- 0 tab-width)) (delete-char -1))))
(global-set-key (kbd "<backspace>") 'cfg:backward-delete-tab-whitespace)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(provide 'emacs-rc-text)
;;; emacs-rc-text.el ends here
