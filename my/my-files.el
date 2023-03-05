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

(provide 'my-files)
