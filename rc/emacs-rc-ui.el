;;; emacs-rc-ui --- Summary

;;; Commentary:
;;; Здесь находятся настройки интерфейса общие для всех режимов

;;; Code:

;;; Тип шрифта
(setq default-frame-alist '((font . "Hack :: Regular: 11:: Cyrillic")))

;; Ник и почта пользователя
(setq user-full-name "Armoken")
(setq user-mail-address "Alex.Armoken@gmail.com")

;; Убрать экраны приветствия
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t)

;; Убрать компоненты GUI
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(setq ring-bell-function 'ignore)

;; Вместо yes и no - y и n
(fset 'yes-or-no-p 'y-or-n-p)

;; Название открытого буфера в шапке окна
(setq frame-title-format "GNU Emacs: %b")

;; Наводим красоту
(fringe-mode '(8 . 0)) ;; ограничить текста только слева
(setq-default indicate-empty-lines t) ;; отсутствие строки выделить глифами рядом с полосой с номером строки
(setq-default indicate-buffer-boundaries 'left) ;; индикация только слева

;; Показывать часы и размер файла
(size-indication-mode t) ;; размер файла в %-ах

;; Fullscreen at startup
(defun fullscreen (&optional f)
	(interactive)
 	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
						   '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
						   '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(when (window-system) (fullscreen))

;; Smooth scroll
(setq scroll-margin 1)
(setq scroll-conservatively 10000)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))

;; Отключить моргание курсора
(blink-cursor-mode 0)

;; Общий с ОС буфер обмена:
(setq x-select-enable-clipboard t)

;; Число строк в mode-line
(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)
(setq-default mode-line-format
              '("  " mode-line-modified
                (list 'line-number-mode "  ")
                (:eval (when line-number-mode
						   (let ((str "L%l"))
							   (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
								   (setq str (concat str "/" my-mode-line-buffer-line-count)))
							   str)))
                "  %p"
                (list 'column-number-mode "  C%c")
                "  " mode-line-buffer-identification
                "  " mode-line-modes))

(defun my-mode-line-count-lines ()
	(setq my-mode-line-buffer-line-count (int-to-string (count-lines (point-min) (point-max)))))

(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)

;; Перемещение между сплитами при помощи комбинаций M-arrow-keys (кроме org-mode)
(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

;; Нумерация строк
(require 'linum) ;; вызвать Linum
(line-number-mode t) ;; показать номер строки в mode-line
(global-linum-mode t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d ") ;; задаем формат нумерации строк

;; Dired (C-x D; Что это что-то вроде файлового менеджера)
(require 'dired)
(setq dired-recursive-deletes 'top) ;; чтобы можно было непустые директории удалять...

;; Быстрая навигация между открытыми буферами
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;; отдельный список буферов при нажатии C-x C-b
(global-set-key (kbd "<f2>") 'bs-show) ;; запуск buffer selection кнопкой F2

;; Imenu
(require 'imenu)
(setq imenu-auto-rescan t) ;; автоматически обновлять список функций в буфере
(setq imenu-use-popup-menu nil) ;; диалоги Imenu только в минибуфере
(global-set-key (kbd "<f6>") 'imenu) ;; вызов Imenu на F6

;; IDO plugin
;; Добавляет и изменяет хоткеи, делая работу продуктивнее
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-virtual-buffers      t)
(setq ido-enable-flex-matching t)

;; Speedbar
(require 'sr-speedbar)
(add-hook 'speedbar-mode-hook
          (lambda ()
              (speedbar-change-initial-expansion-list "quick buffers")))
(setq sr-speedbar-width 40)

(provide 'emacs-rc-ui)
;;; emacs-rc-ui.el ends here
