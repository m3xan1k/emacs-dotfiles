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

(put 'narrow-to-region 'disabled nil)

(provide 'my-sets)
