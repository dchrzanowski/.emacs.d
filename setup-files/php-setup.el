;;; package -- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; PHP
;; --------------------------------------------------------------------
(use-package php-mode
  :defer 10
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               (ac-php-core-eldoc-setup))))

(provide 'php-setup)
;;; php-setup.el ends here
