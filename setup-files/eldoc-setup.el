;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; eldoc
;; --------------------------------------------------------------------
(global-eldoc-mode)

(use-package eldoc-box
  :config
  (add-hook 'prog-mode-hook 'eldoc-box-hover-mode))

(provide 'eldoc-setup)
;;; eldoc-setup ends here
