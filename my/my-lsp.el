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

;; company-mode
(setq company-minimum-prefix-length 1
    company-idle-delay 0.0)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()

  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(add-hook 'go-mode-hook #'lsp-deferred)


;; RACKET
(use-package racket-mode
  :ensure t)

(setq racket-program "/usr/racket/bin/racket")

(provide 'my-lsp)
