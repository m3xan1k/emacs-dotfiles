(global-set-key (kbd "<C-return>") (kbd "C-e C-m"))
(global-set-key (kbd "C-M-b") 'treemacs)
(global-set-key (kbd "<f12>") 'lsp-find-definition)
(global-set-key (kbd "<S-f12>") 'lsp-find-references)
(global-set-key (kbd "C-x C-b") 'bufler)
(define-key global-map (kbd "C-<tab>") 'other-window)
(define-key global-map (kbd "C-z") 'nil)
(global-set-key (kbd "M-s M-s") 'counsel-rg)
(global-set-key (kbd "M-p") 'fzf-projectile)

;; forward-word
(defun my-forward-word ()
  (interactive)
  (if (looking-at "\\W+\n")
    (forward-sexp)
    (forward-word)))

;; backward-word
(defun my-backward-word ()
  (interactive)
  (if (looking-back "\n\\W+")
    (backward-sexp)
    (backward-word)))

(global-set-key (kbd "M-f") 'my-forward-word)
(global-set-key (kbd "M-b") 'my-backward-word)

(provide 'my-keys)
