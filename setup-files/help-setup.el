;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; Helpful
;; --------------------------------------------------------------------
(use-package helpful
  :after evil
  :config
  (evil-set-initial-state 'helpful-mode 'motion))

(use-package zeal-at-point)

(provide 'help-setup)
;;; help-setup.el ends here
