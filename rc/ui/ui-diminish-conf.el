;;; ui-diminish-conf.el --- Summary

;;; Commentary:
;;; UI settings common for all modes

;;; Code:
(require 'diminish)
(diminish 'helm-mode)
(diminish 'abbrev-mode)
(diminish 'visual-line-mode)
(diminish 'smooth-scroll-mode)

(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode "Ⓕ"))
(eval-after-load "hideshow" '(diminish 'hs-minor-mode "ⓗ"))
(eval-after-load "highlight-indentation"
                 '(diminish 'highlight-indentation-mode))

(eval-after-load "elpy" '(diminish 'elpy-mode "Ⓔ"))
(eval-after-load "irony" '(diminish 'irony-mode "Ⓘ"))

(provide 'ui-diminish-conf)
;;; ui-diminish-conf.el ends here
