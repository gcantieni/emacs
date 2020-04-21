;; Packaging system
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Defaults
(setq large-file-warning-threshold 100000000
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      recentf-max-saved-items 5000
      ring-bell-function 'ignore)
(defalias 'yes-or-no-p #'y-or-n-p)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(set-face-attribute 'default nil :height 120)
(show-paren-mode)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; x lines at a time 
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(defun gus-load-module (module-name)
  (load (concat "~/.emacs.d/modules/" module-name ".el")))
(gus-load-module "gus-keybindings")

;; Packages
(use-package diminish)
(use-package ivy
  :diminish t
  :config
  (setq ivy-use-virtual-buffers t
	ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-mode))
(use-package avy)
(use-package projectile
  :diminish t
  :config
  (projectile-mode +1))
(use-package counsel)
(use-package swiper)
(use-package doom-themes
  :config (load-theme 'doom-molokai t))
(use-package magit :commands magit-status)
(use-package company
  :config
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0))
(use-package lsp-mode
  :commands lsp
  :hook (go-mode . lsp-deferred))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package go-mode)
(use-package pdf-tools
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
