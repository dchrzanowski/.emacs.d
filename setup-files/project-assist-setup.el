;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
(use-package projectile
  :config
  (add-hook 'dired-mode-hook #'(lambda () (projectile-mode -1))))

(provide 'project-assist-setup)
;;; project-assist-setup ends here
