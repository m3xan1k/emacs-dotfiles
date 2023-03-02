(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bufler-reverse t)
  '(package-selected-packages
     '(dashboard treemacs-all-the-icons bufler all-the-icons-dired all-the-icons-ivy all-the-icons multiple-cursors move-text expand-region helm fzf ripgrep ag projectile telega ace-window counsel tabbar which-key try company-jedi restclient-jq restclient lsp-treemacs cider clojure-mode yasnippet company lsp-ui go-mode lsp-mode racket-mode true use-package almost-mono-themes lsp-pyright treemacs git-gutter))
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "red1")))))


(set-face-attribute 'default nil :family "Roboto Mono" :height 165)
;;(setq-default cursor-type '(bar . 4))

;; (setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq lisp-indent-offset 2)
(setq column-number-mode t)
(global-display-line-numbers-mode)
(electric-pair-mode t)

(setq visible-bell t)
(setq ring-bell-function 'ignore)
;;(scroll-lock-mode 1)

(setq inhibit-startup-message t)
(tool-bar-mode -1)

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; tab always indent and complete
(setq tab-always-indent 'complete)

;; highlight matching parens
(show-paren-mode 1)

;; trailing whitespace visualize
;;(setq-default show-trailing-whitespace t)

;; backup files
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups" user-emacs-directory))))

;; autosave files
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix
  (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
  auto-save-file-name-transforms
  `((".*" ,(expand-file-name "tmp/auto-saves" user-emacs-directory) t)))

;; lockfiles
(setq create-lockfiles nil)

;; misc
(global-hl-line-mode t)

;; refresh buffer when file changed on disk
(global-auto-revert-mode t)


(require 'package)
(add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
    '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
    '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
    '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package almost-mono-themes
  :ensure t
  :config
  (load-theme 'almost-mono-cream t))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package tabbar
  :ensure t
  :config
  (tabbar-mode 1))

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

;; Company mode
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-Length 1)
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))

;; Lsp mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

;; Python LSP
(use-package lsp-pyright
  :ensure t
  :defer t
  :config
  (setq lsp-pyright-disable-language-service nil
    lsp-pyright-use-library-code-for-types t
    lsp-headerline-breadcrumb-mode t
    lsp-pyright-stub-path (concat (getenv "HOME") "/Documents/projects/python-type-stubs"))

  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))))

;; optionally for LSP
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; git-gutter
(use-package git-gutter
  :ensure t
  :bind
  ("C-x g n" . git-gutter:next-hunk)
  ("C-x g p" . git-gutter:previous-hunk)
  ("C-x g =" . git-gutter:popup-hunk))

(global-git-gutter-mode t)

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

(use-package treemacs
  :ensure t)

(use-package treemacs-all-the-icons
  :ensure t)

(treemacs-load-theme "all-the-icons")

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

;; VARS
(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; company-mode
(setq company-minimum-prefix-length 1
    company-idle-delay 0.0)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()

  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; racket mode
(setq racket-program "/usr/racket/bin/racket")


;; GLOBAL CUSTOM SHORTCUTS
(global-set-key (kbd "<C-return>") (kbd "C-e C-m"))
(global-set-key (kbd "C-M-b") 'treemacs)
(global-set-key (kbd "<f12>") 'lsp-find-definition)
(global-set-key (kbd "<S-f12>") 'lsp-find-references)
(global-set-key (kbd "C-x C-b") 'bufler)
(define-key global-map (kbd "C-<tab>") 'other-window)
(define-key global-map (kbd "C-z") 'nil)
(global-set-key (kbd "M-s M-s") 'counsel-rg)
(global-set-key (kbd "M-p") 'fzf-projectile)
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

(put 'narrow-to-region 'disabled nil)
