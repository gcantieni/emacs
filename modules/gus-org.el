(require 'org)
(setq org-agenda-files '("~/Org/todo.org")
      org-default-notes-dir '("~/Org/notes.org")
      org-agenda-ndays 7
      org-refile-targets (quote (("todo.org" :maxlevel . 1)
				 ("someday.org" :level . 2))
(define-key org-mode-map (kbd "C-c n") 'org-narrow-to-subtree)
(define-key org-mode-map (kbd "C-c N") 'org-widen)

(provide 'gus-org)
