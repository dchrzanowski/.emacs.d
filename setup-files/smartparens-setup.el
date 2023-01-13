;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; smartparens
;; --------------------------------------------------------------------
(use-package smartparens-config
  :ensure smartparens
  :config
  (setq-default sp-escape-quotes-after-insert nil)
  (smartparens-global-mode 1))

(provide 'smartparens-setup)
;;; smartparens-setup.el ends here
