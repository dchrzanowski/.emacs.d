;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; hledger
;; --------------------------------------------------------------------
(use-package hledger-mode
  :bind (("C-c e" . hledger-capture)
         :map hledger-input-mode-map
         ("C-c C-b" . popup-balance-at-point))
  :after company
  :mode ("\\.journal\\'" . hledger-mode)
  :config
  (require 'hledger-input)
  ;; hledger specific
  (setq hledger-jfile (expand-file-name "~/GoogleDrive/org/finance/2023.journal")
        hledger-currency-string " ")
  ;; hledger input specific
  (setq hledger-input-buffer-height 20)
  (add-hook 'hledger-input-post-commit-hook #'hledger-show-new-balances)
  (add-hook 'hledger-input-mode-hook #'auto-fill-mode)
  (add-hook 'hledger-input-mode-hook
            (lambda ()
              (make-local-variable 'company-idle-delay)
              (setq-local company-idle-delay 0.1))))

(use-package flycheck-hledger
  :after (flycheck hledger-mode)
  :demand t)

(provide 'finance-setup)
;;; finance-setup.el ends here
