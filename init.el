(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(set-face-attribute 'default nil :font "JetBrains Mono NL 16")
(setq-default cursor-type '(bar . 4))

;(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq lisp-indent-offset 4)
(setq column-number-mode t)
(global-display-line-numbers-mode)
(electric-pair-mode t)

(setq visible-bell t)
(setq ring-bell-function 'ignore)
;;(scroll-lock-mode 1)

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

(add-to-list 'package-pinned-packages '(telega . "melpa-stable"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    '(package-selected-packages
         '(telega company-jedi restclient-jq restclient lsp-treemacs cider clojure-mode yasnippet company lsp-ui go-mode ## lsp-mode racket-mode true use-package almost-mono-themes lsp-pyright treemacs git-gutter)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(use-package almost-mono-themes
    :config
  ;; (load-theme 'almost-mono-black t))
    ;; (load-theme 'almost-mono-gray t))
    (load-theme 'almost-mono-cream t))
  ;;(load-theme 'almost-mono-white t))

;; -*- mode: elisp -*-

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; tab always indent and complete
(setq tab-always-indent 'complete)

;; racket mode
(setq racket-program "/usr/racket/bin/racket")

;; highlight matching parens
(show-paren-mode 1)

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
;; golang
;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
    
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; LSP mode
;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (python-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

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
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; company-mode
(setq company-minimum-prefix-length 1
    company-idle-delay 0.0)

;; git-gutter
(use-package git-gutter)
(global-git-gutter-mode +1)

;; custom shortcuts
(global-set-key (kbd "<C-return>") (kbd "C-e C-m"))
(global-set-key (kbd "C-M-b") 'treemacs)
(global-set-key (kbd "<f12>") 'lsp-find-definition)
(global-set-key (kbd "<S-f12>") 'lsp-find-references)
