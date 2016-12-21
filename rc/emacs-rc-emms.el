;;; emacs-rc-emms --- Summary

;;; Commentary:
;; Settings only for EMMS

;;; Code:
(eval-when-compile (require 'cl))
(require 'emms-setup)
(emms-all)

(require 'emms-browser)
;; (setq emms-browser-covers "cover_med")
(setq emms-info-asynchronously nil)
(setq emms-playlist-buffer-name "*Music*")
(defvar emms-browser-info-title-format "%A%n")

(require 'emms-source-file)
(require 'emms-playing-time)
(require 'emms-source-playlist)

(setq emms-playlist-default-major-mode 'emms-playlist-mode)
(add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)

;; Info
(require 'emms-info)
(add-to-list 'emms-info-functions 'emms-info-mpd)
(setq emms-info-auto-update t)

;; Lyrics
(require 'emms-lyrics)
(emms-lyrics 1)
(setq emms-lyrics-scroll-p nil)
(setq emms-lyrics-dir "~/.lyrics")
(setq emms-lyrics-display-on-minibuffer t)
(setq emms-lyrics-display-on-modeline nil)

;; MPD
;; This code doesn't run mpd, it just connects to mpd that alive
(eval-when-compile (require 'cl))
(require 'emms-player-mpd)

(add-to-list 'emms-player-list 'emms-player-mpd)
(add-to-list 'emms-info-functions 'emms-info-mpd)

(setq emms-player-mpd-server-port "6600")
(setq emms-player-mpd-server-password "1111")
(setq emms-player-mpd-server-name "127.0.0.1")

(setq emms-player-mpd-sync-playlist t)
(setq emms-player-mpd-music-directory "/mnt/all/Music")
(setq emms-player-mpd-supported-regexp
      (or (emms-player-mpd-get-supported-regexp)
          (concat "\\`http://\\|"
                  (emms-player-simple-regexp
                   "m3u" "ogg" "flac" "mp3" "wav"))))
(emms-player-set emms-player-mpd 'regex emms-player-mpd-supported-regexp)

(provide 'emacs-rc-emms)
;;; emacs-rc-emms.el ends here
