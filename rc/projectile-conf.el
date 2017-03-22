;;; projectile-conf.el --- Summary

;;; Commentary:
;; Projectile config

;;; Code:

(require 'projectile)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-switch-project-action 'helm-projectile)

(setq projectile-enable-caching t)

;; Sepparate buffers for different projectile projects
(require 'perspective)
(persp-mode)

(provide 'projectile-conf.el)
;;; projectile-conf.el ends here
