;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; flycheck linter for all
;; --------------------------------------------------------------------
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-flake8-maximum-line-length 160)
  (flycheck-add-mode 'html-tidy 'web-mode))

(provide 'linting-setup)
;;; linting-setup ends here
