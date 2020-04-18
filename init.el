;; Package system 
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

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

;; Custom general functions
(defun gus-auto-indent ()
  "Auto indent the whole buffer."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-for-tab-command)
    (deactivate-mark)))

;; General
(use-package general)
(general-override-mode)

(general-create-definer gus-spc
  :states 'motion
  :keymaps 'override
  :prefix "SPC")


;; Bind general functions
(gus-spc "i" 'gus-auto-indent)

;; Theme
(defvar gus-dark-theme nil "Determines whether to use dark theme")
; Put on own line so it can be easily manipulated by external program
(setq gus-dark-theme t)
(if gus-dark-theme
    (load-theme 'doom-sourcerer t)
  (load-theme 'doom-nord-light t))

; TODO why can't I do this?
(gus-spc "ttl" '(lambda () "Load light theme" (load-theme 'nimbus nil)))

;; Folding
(use-package folding)

;; Scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; x lines at a time 
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Ivy Counsel
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  :general
  ;('ivy-switch-buffer-map
  ; "," 'ivy-done)
  ('ivy-minibuffer-map
   "TAB" 'ivy-next-line
   "S-TAB" 'ivy-previous-line
   "," 'ivy-done))

(use-package counsel)

;; Multiple cursors
(use-package multiple-cursors)

;; Evil
(use-package evil
  :init
  (setq evil-want-C-d-scroll t
	evil-emacs-state-modes nil)
  (evil-mode 1)
  :config
  (delete 'term-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'term-mode)
  :general
  ('normal "TAB" 'indent-for-tab-command
	   "a" 'evil-append-line
	   "A" 'evil-append)
  ('motion "," nil))

; Make text scale mode apply to all buffers
(defadvice text-scale-increase (around all-buffers (arg) activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ad-do-it)))

;; Keybindings

(defhydra hydra-zoom ()
  "zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out"))


(general-def 'motion
  "C-o" nil)

(general-def 'override
  "C-s" 'save-buffer
  "C-<tab>" 'ivy-switch-buffer
  "C-o" 'other-window
  "C-S-x" 'package-install
  "C-u" 'scroll-down-command
  "C-q" 'save-buffers-kill-terminal)

; Note: Term mode use C-c C-j to switch to line mode

;(general-def 'overriding-terminal-local-map
;  "," 'isearch-exit)

(gus-spc
  "1" 'delete-other-windows
  "0" 'delete-window
  "e" (defhydra hydra-eval ()
	"eval"
	("b" eval-buffer :color blue)
	("s" eval-last-sexp :color blue))
  "s" (defhydra hydra-start ()
	"start app"
	("g" gnus))
  "r" 'revert-buffer
  "x" 'execute-extended-command
  "p" 'package-install
  "f" 'counsel-find-file
  "k" 'kill-buffer
  "TAB" 'counsel-switch-buffer
  "\\" 'split-window-right
  "m" 'imenu
  "=" 'hydra-zoom/body
  "`" 'term)   

(use-package evil-escape
  :diminish t
  :init
  (setq evil-escape-key-sequence "kj")
  (evil-escape-mode))

;; Dired
(use-package dired
  :general
  ('dired-mode-map "SPC" nil
		   "C-o" nil))

;; Completion
(use-package company
  :hook (prog-mode . company-mode)
  :general
  (company-active-map
  q "TAB" #'company-complete-common-or-cycle
   [tab] #'company-complete-common-or-cycle
   ;; don't take over these keys
   "C-h" nil
   "C-w" nil))


;; Langauge Server Protocol LSP

(use-package lsp-mode
  :hook (js-mode . lsp))

(use-package company-lsp
  :after company
  :config
  (add-to-list 'company-backends 'company-lsp))

;; Text
(remove-hook 'text-mode-hook #'turn-on-auto-fill) 
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) 

;; Org

(use-package org
  :init
  (add-hook 'org-mode-hook #'org-indent-mode)
  (gus-spc
    "c" 'org-capture
    "a" 'org-agenda)
  :config
  (setq org-directory "~/Documents/Brain"
	org-support-shift-select t
        org-link-file-path-type 'relative
        org-default-notes-file (concat org-directory "/gtd.org")
        org-agenda-files '("~/Documents/Brain/task.org")
        org-todo-keywords '((sequence "TODO(t)"
				      "NEXT(n)"
				      "PROJECT(p)"
				      "SOMEDAY(s)"
				      "|"
				      "DONE(d)"
				      "CANCELLED(c)"))
        org-archive-location "~/Documents/Brain/archive.org::"
	; Ricing
	org-hide-emphasis-markers t
	org-agenda-block-separator ""
	org-ellipsis " › "
	org-pretty-entities t
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t

	; Capture
	org-capture-templates '(("t" "Todo [inbox]" entry (file "task.org") "* TODO %i%?")
				("p" "Project" entry (file "task.org") "* PROJECT %i%?")
				("j" "Journal" entry (file+datetree "think.org") "* %? %t")))



  (defun gus-org-insert-heading ()
    "Insert heading with newline."
    (interactive)
    (evil-append-line 1)
    (newline)
    (org-insert-heading))

  (defun gus-org-insert-todo ()
    "Insert a todo heading."
    (interactive)
    (gus-org-insert-heading)
    (org-todo)
    (evil-append-line 1))

  :general
  ('org-mode-map
   "M-h" 'org-metaleft
   "M-j" 'org-metadown
   "M-k" 'org-metaup
   "M-l" 'org-metaright)
  ('normal
   'org-mode-map
   :prefix ","
   "a" 'org-archive-subtree
   ; todo keywords
   "t" 'org-todo
   "1" '(lambda () (interactive) (org-todo 1))
   "2" '(lambda () (interactive) (org-todo 2))
   "3" '(lambda () (interactive) (org-todo 3))
   "4" '(lambda () (interactive) (org-todo 4))
   "5" '(lambda () (interactive) (org-todo 5))
   "6" '(lambda () (interactive) (org-todo 6))
   "7" '(lambda () (interactive) (org-todo 7))
   "s" 'org-schedule
   "d" 'org-deadline
   "." 'org-time-stamp
   "e" 'org-export-dispatch
   "k" 'org-set-tags
   "n" 'org-narrow-to-subtree
   "N" 'widen
   "i" 'gus-org-insert-todo
   "h" 'gus-org-insert-heading
   "c" 'org-toggle-checkbox
   "r" 'org-refile)
  ;; Org Agenda
  ('org-agenda-mode-map 'motion "r" nil)
  ('org-agenda-mode-map
   "," ;; Hydra for org agenda (graciously taken from Spacemacs)
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line
   "SPC" nil))

(use-package worf
  :after org)

(use-package plain-org-wiki
  :init
  (gus-spc "w" 'plain-org-wiki)
  :config
  
  (setq plain-org-wiki-directory "~/Documents/Brain/wiki"
	gus-wiki-files (mapcar 'cdr (plain-org-wiki-files))
	org-refile-targets '(("~/Documents/Brain/task.org" :level . 1)
			     ("~/Documents/Brain/think.org" :level . 1)
			     (gus-wiki-files :level . 1)))
  :commands plain-org-wiki)

(use-package org-bullets
  :init
  (setq org-bullets-bullet-list '("•"))
  :hook (org-mode . org-bullets-mode))

;; JavaScript

(use-package js
  :config
  (setq js-indent-level 2))

;; Html

(use-package web-mode
  :init
  ;; useful optionos: web-mode-enable-current-column-highlight t
  (setq web-mode-enable-current-element-highlight t
	web-mode-enable-auto-closing t
	web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2)
  :mode "\\.html?\\'"
  )

;; Startup message (put at the end to indicate if anything before it failed silently)
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;; Custom garbage
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("07e3a1323eb29844e0de052b05e21e03ae2f55695c11f5d68d61fb5fed722dd2" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "ca849ae0c889eb918785cdc75452b1e11a00848a5128a95a23872e0119ccc8f4" "ec8246f6f74bfe0230521412d88092342c17c1c0448a4b8ba39bddd3da170590" "774aa2e67af37a26625f8b8c86f4557edb0bac5426ae061991a7a1a4b1c7e375" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "88a3c267ce2132defd46f2a4761925983dcbc35b1c3cfff1dded164ce169fed4" "0809c08440b51a39c77ec5529f89af83ab256a9d48107b088d40098ce322c7d8" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "559b28ae6deb74713fee9064e7ece54cb71ba645f44acbf81ad7916a4f947815" "e14e8e4ecae1b4e0983367c6ab7437720f10b85865549b9ea329be2c2eaa9e80" default)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (multiple-cursors doom-themes minimal-theme dashboard web-mode plain-org-wiki worf diminish evil-escape folding yasnippet use-package treemacs swiper nimbus-theme jedi general evil company-lsp centaur-tabs all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
