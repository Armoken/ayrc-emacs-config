;;; projectile-conf.el --- Summary

;;; Commentary:
;; Projectile config

;;; Code:


(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-switch-project-action 'helm-projectile)

(setq projectile-enable-caching t)



(provide 'projectile-conf.el)
;;; projectile-conf.el ends here
