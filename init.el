(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setq custom-file (expand-file-name "~/.emacs.d/my/my-custom.el"))
(load custom-file)

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

(add-to-list 'load-path "~/.emacs.d/my")

(require 'my-files)
(require 'my-sets)
(require 'my-keys)
(require 'my-tools)
(require 'my-ui)
(require 'my-lsp)
(require 'my-org)
