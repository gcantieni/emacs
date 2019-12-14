;; Package system 
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Defaults 
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(set-face-attribute 'default nil :height 120)
(setq large-file-warning-threshold 100000000)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq recentf-max-saved-items 5000)
(show-paren-mode)
(defalias 'yes-or-no-p #'y-or-n-p)
(defvar gus-dark-theme nil "Determines whether to use dark theme")
(setq ring-bell-function 'ignore)
(if gus-dark-theme
    (load-theme 'nimbus t)
  (load-theme 'adwaita t))
(use-package folding)


;; "General" keybindings
(use-package general)
(general-override-mode)

(general-create-definer gus-spc
  :states 'motion
  :keymaps 'override
  :prefix "SPC")


(defhydra hydra-zoom ()
  "zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out"))

;TODO
;(defhydra hydra-resize ()
;  "resize"
;  ("]" 

(general-def 'motion
  "C-o" nil)

(general-def 'override
  "C-s" 'save-buffer
  "C-<tab>" 'ivy-switch-buffer
  "C-o" 'other-window
  "C-S-x" 'package-install
  "C-q" 'save-buffers-kill-terminal)

;(general-def 'overriding-terminal-local-map
;  "," 'isearch-exit)

(gus-spc
  "1" 'delete-other-windows
  "0" 'delete-window
  "e" (defhydra hydra-eval ()
	"eval"
	("b" eval-buffer :color blue)
	("s" eval-last-sexp :color blue))
  "r" 'revert-buffer
  "x" 'execute-extended-command
  "p" 'package-install
  "f" 'counsel-find-file
  "k" 'kill-buffer
  "TAB" 'counsel-switch-buffer
  "\\" 'split-window-right
  "i" 'imenu
  "=" 'hydra-zoom/body
  "`" 'term)   

;; Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
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

;; Evil
(use-package evil
  :init
  (evil-mode 1)
  :config
  :general
  ('normal "TAB" 'indent-for-tab-command
	   "a" 'evil-append-line
	   "A" 'evil-append)
  ('motion "," nil) )

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

;; Org

(use-package org
  :init
  (add-hook 'org-mode-hook #'org-indent-mode)
  (gus-spc
    "c" 'org-capture
    "a" 'org-agenda)
  :config
  (setq org-directory "~/Dropbox/Brain"
        org-link-file-path-type 'relative
        org-default-notes-file (concat org-directory "/gtd.org")
        org-agenda-files '("~/Dropbox/Brain/task.org")
        org-todo-keywords '((sequence "TODO(t)"
				      "|"
				      "DONE(d)"
				      "CANCELLED(c)"))
        org-archive-location "~/Dropbox/Brain/archive.org::"
	; Ricing
	org-hide-emphasis-markers t
	org-agenda-block-separator ""
	org-ellipsis " › "
	org-pretty-entities t
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t

	; Capture
	org-capture-templates '(("t" "Todo [inbox]" entry (file+headline "task.org" "Inbox") "* %i%?")
				("p" "Project" entry (file+headline "task.org" "Projects") "* %i%?")
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
   "t" 'org-todo
   "s" 'org-schedule
   "d" 'org-deadline
   "." 'org-time-stamp
   "e" 'org-export-dispatch
   "k" 'org-set-tags
   "n" 'org-narrow-to-sub
   "N" 'widen
   "i" 'gus-org-insert-todo
   "h" 'gus-org-insert-heading
   "c" 'org-toggle-checkbox
   "r" 'org-refile)
  ;; Org Agenda
  ('org-agenda-mode-map
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line
   "," ;; Hydra for org agenda (graciously taken from Spacemacs)
   (defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
				    :post (setq which-key-inhibit nil)
				    :hint none)
     "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
     ;; Entry
     ("hA" org-agenda-archive-default)
     ("hk" org-agenda-kill)
     ("hp" org-agenda-priority)
     ("hr" org-agenda-refile)
     ("h:" org-agenda-set-tags)
     ("ht" org-agenda-todo)
     ;; Visit entry
     ("o"   link-hint-open-link :exit t)
     ("<tab>" org-agenda-goto :exit t)
     ("TAB" org-agenda-goto :exit t)
     ("SPC" org-agenda-show-and-scroll-up)
     ("RET" org-agenda-switch-to :exit t)
     ;; Date
     ("dt" org-agenda-date-prompt)
     ("dd" org-agenda-deadline)
     ("+" org-agenda-do-date-later)
     ("-" org-agenda-do-date-earlier)
     ("ds" org-agenda-schedule)
     ;; View
     ("vd" org-agenda-day-view)
     ("vw" org-agenda-week-view)
     ("vt" org-agenda-fortnight-view)
     ("vm" org-agenda-month-view)
     ("vy" org-agenda-year-view)
     ("vn" org-agenda-later)
     ("vp" org-agenda-earlier)
     ("vr" org-agenda-reset-view)
     ;; Toggle mode
     ("ta" org-agenda-archives-mode)
     ("tA" (org-agenda-archives-mode 'files))
     ("tr" org-agenda-clockreport-mode)
     ("tf" org-agenda-follow-mode)
     ("tl" org-agenda-log-mode)
     ("td" org-agenda-toggle-diary)
     ;; Filter
     ("fc" org-agenda-filter-by-category)
     ("fx" org-agenda-filter-by-regexp)
     ("ft" org-agenda-filter-by-tag)
     ("fr" org-agenda-filter-by-tag-refine)
     ("fh" org-agenda-filter-by-top-headline)
     ("fd" org-agenda-filter-remove-all)
     ;; Clock
     ("cq" org-agenda-clock-cancel)
     ("cj" org-agenda-clock-goto :exit t)
     ("ci" org-agenda-clock-in :exit t)
     ("co" org-agenda-clock-out)
     ;; Other
     ("q" nil :exit t)
     ("gd" org-agenda-goto-date)
     ("." org-agenda-goto-today)
     ("gr" org-agenda-redo))))


(use-package worf
  :after org)

(use-package plain-org-wiki
  :init
  (gus-spc "w" 'plain-org-wiki)
  :config
  
  (setq plain-org-wiki-directory "~/Dropbox/Brain/wiki"
	gus-wiki-files (mapcar 'cdr (plain-org-wiki-files))
	org-refile-targets '(("~/Dropbox/Brain/task.org" :level . 1)
			     ("~/Dropbox/Brain/think.org" :level . 1)
			     (gus-wiki-files :level . 1)))
  :commands plain-org-wiki)

(use-package org-bullets
  :init
  (setq org-bullets-bullet-list '("•"))
  :hook (org-mode . org-bullets-mode))
  
;; Custom garbage
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e14e8e4ecae1b4e0983367c6ab7437720f10b85865549b9ea329be2c2eaa9e80" default)))
 '(package-selected-packages
   (quote
    (plain-org-wiki worf diminish evil-escape folding yasnippet use-package treemacs swiper nimbus-theme jedi general evil company-lsp centaur-tabs all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
