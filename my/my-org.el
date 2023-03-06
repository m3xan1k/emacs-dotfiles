(use-package org
  :ensure t)

(use-package org-bullets
  :ensure t)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(provide 'my-org)
