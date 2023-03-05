(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (global-set-key (kbd "C-s") 'swiper)
    (global-set-key (kbd "C-r") 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package treemacs
  :ensure t)

(use-package projectile
  :ensure t
  :pin melpa-stable
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-x p" . projectile-command-map)))

(use-package ag
  :ensure t)

(use-package ripgrep
  :ensure t)

(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
    fzf/window-height 15))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-d") 'mc/mark-next-like-this-word)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "<M-s-down>") 'mc/edit-beginnings-of-lines))

(use-package bufler
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                           (bookmarks . 5)
                           (projects . 5)
                           (agenda . 5)
                           (registers . 5))))

(use-package restclient
  :ensure t)

(use-package restclient-jq
  :ensure t)

;; git-gutter
(use-package git-gutter
  :ensure t
  :bind
  ("C-x g n" . git-gutter:next-hunk)
  ("C-x g p" . git-gutter:previous-hunk)
  ("C-x g =" . git-gutter:popup-hunk))

(global-git-gutter-mode t)

;; telega
(use-package telega
  :ensure t
  :config
  (setq telega-use-docker t)
  (setq telega-enable-storage-optimizer t)
  (setq telega-chat-input-markups '("markdown2" nil "org"))
  (add-hook 'telega-load-hook
    (lambda ()
      (define-key global-map (kbd "C-c t") telega-prefix-map))))

(provide 'my-tools)
