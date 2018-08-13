;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;;; ledger
;; --------------------------------------------------------------------
(use-package ledger-mode)

(use-package evil-ledger
  :after ledger-mode
  :config
  (add-hook 'ledger-mode-hook #'evil-ledger-mode))

(provide 'accounting-setup)
;;; accounting-setup ends here
