;;; ui-helm-conf.el --- Summary

;;; Commentary:
;; Settings for helm

;;; Code:

(require 'helm)
(require 'helm-config)

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
    (setq helm-grep-default-command
          "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command
          "ack-grep -H --no-group --no-color %e %p %f"))

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; Open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p t)

;; Move to end or beginning of source when reaching top or bottom of source.
(setq helm-move-to-line-cycle-in-source t)

;; Search for library in `require' and `declare-function' sexp.
(setq helm-ff-search-library-in-sexp t)

;; Scroll 8 lines other window using M-<next>/M-<prior>
(setq helm-scroll-amount 8)

(setq helm-ff-file-name-history-use-recentf t)

;; Helm-flycheck
(require 'helm-flycheck) ;; Not necessary if using ELPA package
(eval-after-load 'flycheck
                 '(define-key flycheck-mode-map
                   (kbd "C-c ! h") 'helm-flycheck))

;; Helm-company
(require 'company)
(autoload 'helm-company "helm-company")
(eval-after-load 'company
                 '(progn
                   (define-key company-mode-map (kbd "C-:") 'helm-company)
                   (define-key company-active-map (kbd "C-:") 'helm-company)))

(helm-mode 1)

(provide 'ui-helm-conf)
;;; ui-helm-conf.el ends here
