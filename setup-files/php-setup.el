;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; Python
;; --------------------------------------------------------------------
(use-package php-mode
  :config
  (use-package company-php
    :config
    (add-hook 'php-mode-hook
              '(lambda ()
                 (require 'company-php)
                 (company-mode t)
                 (ac-php-core-eldoc-setup) ;; enable eldoc
                 (make-local-variable 'company-backends)
                 (add-to-list 'company-backends 'company-ac-php-backend)))))

(provide 'php-setup)
;;; php-setup ends here
