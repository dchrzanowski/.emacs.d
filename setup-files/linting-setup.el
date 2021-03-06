;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; flymake
;; --------------------------------------------------------------------
(setq flymake-no-changes-timeout 1)

;; NOTE: Temporarily disabled flycheck, its a bit slow
;; --------------------------------------------------------------------
;; flycheck linter for all
;; --------------------------------------------------------------------
;; (use-package flycheck
;;   :ensure-system-package (tidy . "sudo pacman -S tidy")
;;   :config
;;   (global-flycheck-mode)
;;   (setq-default flycheck-flake8-maximum-line-length 160)
;;   (flycheck-add-mode 'html-tidy 'web-mode))

;; --------------------------------------------------------------------
;; flycheck-posframe
;; --------------------------------------------------------------------
;; (use-package flycheck-posframe
;;   :ensure t
;;   :after flycheck
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(provide 'linting-setup)
;;; linting-setup ends here
