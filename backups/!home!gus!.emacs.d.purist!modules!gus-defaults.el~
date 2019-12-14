(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(setq large-file-warning-threshold 100000000)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq recentf-max-saved-items 5000)
(show-paren-mode)
(defalias 'yes-or-no-p #'y-or-n-p)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(provide 'gus-defaults)
