;;; emacs-rc-helm --- Summary

;;; Commentary:
;; Settings for helm

;;; Code:

(require 'helm)
(require 'helm-config)

(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c x") 'helm-register)
(global-set-key (kbd "C-x f") 'helm-find-files)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key helm-map (kbd "C-q") 'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;; Fuzzy helm (approximate search)
(require 'helm-dabbrev)
(require 'helm-command)
(require 'helm-semantic)
(setq helm-M-x-fuzzy-match t
	  helm-imenu-fuzzy-match t
      helm-apropos-fuzzy-match t
	  helm-recentf-fuzzy-match t
      helm-locate-fuzzy-match nil
	  helm-semantic-fuzzy-match t
	  helm-lisp-fuzzy-completion t
      helm-buffers-fuzzy-matching t
	  helm-completion-in-region-fuzzy-match t)

;; Need for helm-google-suggest
(when (executable-find "curl")
	(setq helm-net-prefer-curl t))

(when (executable-find "ack-grep")
	(setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
		  helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	  helm-ff-file-name-history-use-recentf t)

;; Helm-flycheck
(require 'helm-flycheck) ;; Not necessary if using ELPA package
(eval-after-load 'flycheck
				 '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; Helm-company
(require 'company)
(autoload 'helm-company "helm-company")
(eval-after-load 'company
				 '(progn
				   (define-key company-mode-map (kbd "C-:") 'helm-company)
				   (define-key company-active-map (kbd "C-:") 'helm-company)))

(helm-mode 1)

(provide 'emacs-rc-helm)
;;; emacs-rc-helm.el ends here
