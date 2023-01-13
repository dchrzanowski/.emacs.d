;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; paradox
;; --------------------------------------------------------------------
(use-package paradox
  :defer 15)

(use-package esup
  :defer 1
  :config
  (evil-set-initial-state 'esup-mode 'motion))

(provide 'package-assistants-setup)
;;; package-assistants-setup.el ends here
