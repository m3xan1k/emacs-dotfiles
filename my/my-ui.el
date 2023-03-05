
(set-face-attribute 'default nil :family "Roboto Mono" :height 165)

(use-package almost-mono-themes
  :ensure t
  :config
  (load-theme 'almost-mono-cream t))

;; icons
(use-package all-the-icons
  :ensure t
  :defer 0.5)

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands
            '(ivy-switch-buffer-other-window
               ivy-switch-buffer))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :ensure t)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package treemacs-all-the-icons
  :ensure t)

(treemacs-load-theme "all-the-icons")

(use-package beacon
  :ensure t
  :init (beacon-mode 1)
  :config
  (setq beacon-blink-when-point-moves t)
  (setq beacon-blink-when-window-changes t)
  (setq beacon-color "gray"))

;; tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(centaur-tabs-headline-match)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-plain-icons t)
(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'over)
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "*")

(provide 'my-ui)
