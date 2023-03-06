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

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; tab always indent and complete
(setq tab-always-indent 'complete)

;; highlight matching parens
(show-paren-mode 1)

;; misc
(global-hl-line-mode t)

;; refresh buffer when file changed on disk
(global-auto-revert-mode t)

(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; enable narrow for region
(put 'narrow-to-region 'disabled nil)

;; fix scroll
(setq scroll-step 1)
(setq scroll-preserve-screen-position 'always)

;; remember cursor position when open file again
(save-place-mode 1)

;; recent files history
(recentf-mode 1)

;; change buffer when file changes on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Kill \n also if killing from the begining of line
(setq kill-whole-line t)

;; word-forward without delimiters
(global-superword-mode t)

;; hide toolbar
(tool-bar-mode -1)

(provide 'my-sets)
