;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; hledger
;; --------------------------------------------------------------------
(use-package hledger-mode
  :after company
  :mode ("\\.journal\\'" . hledger-mode)
  :config
  (setq hledger-jfile (expand-file-name "~/GoogleDrive/org/finance/2023.journal")
        hledger-currency-string " "))

(use-package flycheck-hledger
  :after (flycheck ledger-mode)
  :demand t)

(provide 'finance-setup)
;;; finance-setup.el ends here
