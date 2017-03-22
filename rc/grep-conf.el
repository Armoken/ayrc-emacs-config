;;; grep-conf.el --- Summary

;;; Commentary:
;; Settings for grep, ack, ag

;;; Code:

(defvar ack-history nil
  "History for the `ack' command.")

(defun ack (command-args)
    "Minimalistic interface to ACK.
'COMMAND-ARGS' - obviously"
    (interactive
     (let ((ack-command "ack --nofilter --nogroup --with-filename "))
         (list (read-shell-command "Run ack (like this): "
                                   ack-command
                                   'ack-history))))
    (let ((compilation-disable-input t))
        (compilation-start (concat command-args " < " null-device)
                           'grep-mode)))


(provide 'grep-conf.el)
;;; grep-conf.el ends here
