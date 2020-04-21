;; Unprefixed bindings
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)

;; C-x bindings
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer) ; Sometimes mis-type
(global-set-key (kbd "C-x g") 'magit)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
;; C-c bindings
(global-set-key (kbd "C-c x") 'package-install)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c eb") 'eval-buffer)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c i") 'imenu)


