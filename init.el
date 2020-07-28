;; Packaging system
(defvar bootstrap-version)
(setq straight-check-for-modifications '(watch-files find-when-checking))
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

(setq gc-cons-threshold (* 50 1000 1000)) ;; Performance
(setq large-file-warning-threshold 100000000) ;; Open big shit
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq recentf-max-saved-items 5000) ;; Save lots of stuff
(setq ring-bell-function 'ignore) ;; Stop flashing
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; x lines at a time 
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq inhibit-startup-screen t)
(setq-default tab-width 4)
(defalias 'yes-or-no-p #'y-or-n-p)

;; Modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(show-paren-mode)
(global-visual-line-mode 1)

(defun gus-load-module (module-name)
  (load (concat "~/.emacs.d/modules/" module-name ".el")))
(gus-load-module "gus-keybindings")
;;(gus-load-module "gus-org")

;; Packages
;; Everything that's just a few lines goes here.
;; Anything longer goes in it's own module.

(use-package hydra)

(use-package rg)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-directory "~/Org")
  (org-support-shift-select t)
  (org-agenda-files
   '("~/Org/Gtd/todo.org"
	 "~/Org/Gtd/waiting.org"))
  (org-archive-location "~/Org/Gtd/archive.org::")
  (org-default-notes-dir '("~/Org/notes.org"))
  (org-todo-keywords
   '((sequence
	  "IN(i)" "TODO(t)" "WAITING(w)"
	  "|" "DONE(d)" "CANCELLED(c)")))
  (org-capture-templates
   '(("i" "Inbox" entry (file+headline "~/Org/Gtd/todo.org" "Inbox") "* IN %?")
	 ("p" "Project" entry (file+headline "~/Org/Gtd/todo.org" "Projects") "* PROJECT %?\nEntered %u")))
  (org-agenda-custom-commands
   '(("n" "Next actions" todo "TODO" ((org-agenda-overriding-header "Next actions")))
	 ("i" "Inbox" todo "IN" ((org-agenda-overriding-header "Inbox")))
	 ("w" "Waiting" todo "WAITING" ((org-agenda-overriding-header "Waiting tasks")))
	 ("d" "Done" todo "DONE|CANCELLED" ((org-agenda-overriding-header "Non actionable tasks")))))
  (org-stuck-projects '("/+PROJECT" ("TODO" "WAITING") nil ""))
  (org-refile-targets (quote (("todo.org" :maxlevel . 3) ("waiting.org" :maxlevel . 3) ("someday.org" :level . 1))))
  (org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-agenda-ndays 7)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-start-on-weekday nil)
  ;; Ricing
  (org-hide-emphasis-markers t)
  (org-agenda-block-separator "")
  (org-ellipsis " › ")
  (org-pretty-entities t)
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-babel-load-languages '((emacs-lisp . t) (abc . t))) ; Add support for abc notation
  :config
  (defhydra hydra-gus-process-inbox (org-agenda-mode-map "J")
	"Process inbox"
	("q" nil "quit"))

  (defun gus-new-reference-from-heading (reftype)
	"Make new piece of reference information based on an org heading."
	(interactive "cMake a new reference for a (p)roject, (a)rea or (r)resource")
	(let ((heading-text (nth 4 (org-heading-components)))
		  (area-dir (expand-file-name "~/Org/Reference/1 Areas/"))
		  (project-dir (expand-file-name "~/Org/Reference/2 Projects/"))
		  (resource-dir (expand-file-name "~/Org/Reference/3 Resources/"))
		  (refdir ""))
	  (cond ((char-equal reftype ?p) (setq refdir project-dir))
			((char-equal reftype ?a) (setq refdir area-dir))
			((char-equal reftype ?r) (setq refdir resource-dir))
			(t (error "Must be of type project, area, or resource")))
	  (mkdir (concat refdir heading-text) t)
	  (find-file (concat refdir heading-text "/" heading-text ".org"))
	  (insert (concat "#+TITLE: " heading-text))))

  (defun gus-goto-reference ()
	"Go to a reference org file interactively.
Can be narrowed by area, project, resource, or archive."
	(interactive))
  
  (defhydra hydra-reference (global-map "C-c r")
	"Manipulate reference information"
    ("n" gus-new-reference))

  (defun gus-lookup-tune-from-heading ()
	"From an org heading, lookup on thesession.org."
	(interactive)
	(let ((tune-name (nth 4 (org-heading-components))))
	  (browse-url (gus--tune-query-string tune-name))))

  (defun gus--tune-query-string (tune-name)
	"Given string TUNE-NAME, build a query string for thesession.org."
	(let* ((tune-words (split-string tune-name))
		   (query (car tune-words))
		   (rest (cdr tune-words)))
	  (dolist (word rest query)
		(setq query (concat query "+" word)))
	  (setq query (concat "https://thesession.org/tunes/search?q=" query))))

  (browse-url (gus--tune-query-string "Idle jig"))
		  
	 
  :bind (("C-c a" . org-agenda)
		 ("C-c i" . (lambda () (interactive) (org-capture nil "i")))
		 ("C-c l" . org-store-link)
		 ("C-c z n" . gus-new-zettel)
		 :map org-mode-map
			  ("C-o" . nil)
			  ("C-<tab>" . nil)
		 :map org-agenda-mode-map
			  ("G" . hydra-gus-process-inbox/body)))

(use-package deft
  :after org
  :custom
  (deft-directory "~/Org/Zettelkasten"))

(use-package org-roam
  :config
  (defun gus-insert-resource ()
	"Select a resource from the resources folder and insert a link
to it in the current buffer."
	(interactive)
	(ivy-read
	 "Choose resource:"
	 (directory-files "~/Org/Brain/1 Resources")
	 :action
	 (lambda (x)
	   (org-insert-link nil (concat "file:1 Resources/" x) x))))
  :custom (org-roam-directory "~/Org/Zettelkasten")
  :commands org-roam-find-file
  :hook (after-init . org-roam-mode)
  :bind (("C-c n f" . #'org-roam-find-file)
		 :map org-roam-mode-map
			  ("C-c n i" . 'org-roam-insert)
			  ("C-c n r" . 'gus-insert-resource)
			  ("C-c n b" . 'org-roam-switch-to-buffer)
			  ("C-c n 1" . 'org-roam)
			  ("C-c n g" . 'org-roam-graph)))

;;(require 'org-roam-protocol)
(use-package org-download :after org)

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Org/Brain")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package diminish)

(use-package ivy
  :diminish t
  :config
  (setq ivy-use-virtual-buffers t
		ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)))

(use-package counsel
  :bind (("C-x C-f" . #'counsel-find-file)
		 ("C-<tab>" . #'counsel-switch-buffer)
		 ("M-x" . #'counsel-M-x)
		 ("C-`" . counsel-switch-buffer-other-window)
		 ("C-q t" . #'counsel-load-theme)))

(use-package avy
  :bind (("C-;" . avy-goto-char)
		 ("C-'" . avy-goto-char2)))

(use-package projectile
  :diminish t
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode +1))

(use-package swiper) ;; Search in a cool way

(use-package doom-themes
  :init
  (setq gus-dark-theme 'doom-city-lights) ; doom molokai)
  (setq gus-light-theme 'doom-one-light)
  (setq gus-current-theme gus-dark-theme)
  (defun gus-toggle-theme ()
	"Toggle between dark theme and light theme"
	(interactive)
	(if (eq gus-current-theme gus-dark-theme)
		(load-theme (setq gus-current-theme gus-light-theme) t)
	  (load-theme (setq gus-current-theme gus-dark-theme) t)))
  
  (load-theme gus-current-theme t)
  
  :bind ("C-c t t" . gus-toggle-theme))

(use-package magit
  :commands magit-status
  :bind (:map magit-mode-map
			  ("C-<tab>" . nil)))

(use-package company
  :config
  (setq company-minimum-prefix-length 1
		company-idle-delay 0.2))

(use-package lsp-mode
  :commands lsp
  :hook (go-mode . lsp-deferred)
  :config
  (defun lsp-go-install-save-hooks ()
	(add-hook 'before-save-hook #'lsp-format-buffer t t)
	(add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package flycheck-inline
  :config
  (with-eval-after-load 'flycheck
	(add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))

(use-package company-lsp :commands company-lsp)

(use-package go-mode :mode "\\.go\\'")

(use-package gotest
  :after go-mode
  :bind (:map go-mode-map
			  ("C-c t p" . 'go-test-current-project)
			  ("C-c t t" . 'go-test-current-test)
			  ("C-c t f" . 'go-test-current-file)
			  :map go-test-mode-map
			  ("C-o" . nil)))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-loader-install)
  :bind (:map pdf-view-mode-map
			  ("C-s" . 'isearch-forward)))

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))

(use-package multiple-cursors
  :bind (("C->" . #'mc/mark-next-like-this)
		 ("C-<" . #'mc/mark-previous-like-this)))

(use-package leetcode)
;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook))
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; END
