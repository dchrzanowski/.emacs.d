;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; Projectile setup for .dir-locals.el
;; --------------------------------------------------------------------
;; ((nil . ((projectile-project-run-cmd . "mvn clean compile exec:java")
;;          (projectile-project-test-cmd . "mvn clean compile test"))))

;; --------------------------------------------------------------------
;; Projectile
;; --------------------------------------------------------------------
(use-package projectile
  :config
  (setq compilation-scroll-output t
        projectile-run-use-comint-mode t
        projectile-test-use-comint-mode t)
  (add-hook 'dired-mode-hook #'(lambda () (projectile-mode -1))))

(provide 'project-assist-setup)
;;; project-assist-setup.el ends here
