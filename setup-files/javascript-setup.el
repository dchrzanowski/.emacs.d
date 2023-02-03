;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; js-doc
;; --------------------------------------------------------------------
(use-package js-doc
  :defer 3)

;; --------------------------------------------------------------------
;; js2 mode
;; --------------------------------------------------------------------
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))  ;; attach js2 mode to js files


  (add-hook 'js2-mode-hook (lambda()
                             (hl-todo-mode)
                             (company-mode))))

(provide 'javascript-setup)
;;; javascript-setup.el ends here
