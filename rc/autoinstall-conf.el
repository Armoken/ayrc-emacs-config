;;; autoinstall-conf.el --- Summary

;;; Commentary:
;; Package auto install

;; About all installed packages:
;; assync - Asynchronous processing in Emacs

;; auto-complete - Auto Completion for GNU Emacs (Omnisharp)

;; cl-lib - Common Lisp extensions for Emacs
;; (Company, SLIME, coffe-mode, js2-mode)

;; dash - A modern list api for Emacs.  No cl require
;; (Flycheck, git)

;; dash-functional - Collection of useful combinators for
;; Emacs Lisp (company-tern)

;; epl - EPL provides a convenient high-level API for
;; various package.el versions, and aims to overcome its
;; most striking idiocies.

;; f - Modern API for working with files and directories
;; (Git)

;; find-file-in-project - Find files in a project quickly,
;; on any OS (Elpy)

;; json - JavaScript Object Notation parser/generator (Tern)
;; highlight-indentation -  Minor modes for highlighting
;; indentation (Elpy)

;; let-alist - Easily let-bind values of an assoc-list by
;; their names (Flycheck)

;; pkg-info - Information about packages (Flycheck)

;; popup - Visual Popup User Interface

;; pyvenv - Python virtual environment interface (Elpy)

;; s - The long lost Emacs string manipulation library.(Git)

;; seq - Sequence manipulation functions (Flycheck)

;; swiper - Gives you an overview as you search for a regex
;; (find-file-in-project (Elpy))

;; thingatpt - The library can determine and retrieve
;; different kinds of thing (symbol, list, S-expression
;; (sexp), function definition) at point.

;;; Code:

(defvar my-packages-list
  '(
    async
    auctex
    auto-complete
    clang-format
    cmake-ide
    cmake-mode
    company
    company-auctex
    company-flx
    company-irony
    company-irony-c-headers
    company-math
    company-quickhelp
    company-tern
    company-web
    csharp-mode
    dash
    dash-functional
    diminish
    ecb
    elpy
    emmet-mode
    emms
    emms-info-mediainfo
    emms-mode-line-cycle
    emms-player-mpv
    emms-state
    epl
    f
    find-file-in-project
    flx
    flycheck
    flycheck-irony
    google
    google-c-style
    helm
    helm-core
    helm-css-scss
    helm-emmet
    helm-emms
    helm-firefox
    helm-flycheck
    helm-flyspell
    helm-projectile
    highlight-indentation
    irony
    ivy
    js2-mode
    json-mode
    json-reformat
    json-snatcher
    let-alist
    levenshtein
    macrostep
    markdown-mode
    math-symbol-lists
    matlab-mode
    mpg123
    nasm-mode
    omnisharp
    org
    pkg-info
    popup
    pos-tip
    powerline
    projectile
    pyvenv
    rtags
    s
    seq
    slime
    slime-company
    spaceline
    spacemacs-theme
    sqlup-mode
    sr-speedbar
    swiper
    sublimity
    tern
    undo-tree
    wcheck-mode
    web-completion-data
    web-mode
    yasnippet))

(defun my-auto-install-packages ()
    "Function for package auto installation."
    (package-refresh-contents)
    (mapc #'(lambda (package)
                (unless (package-installed-p package)
                    (package-install package)))
          my-packages-list)
    (save-buffers-kill-emacs))

(provide 'autoinstall-conf.el)
;;; autoinstall-conf.el ends here
