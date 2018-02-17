;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; ggtags
;; --------------------------------------------------------------------
(use-package ggtags)

;; --------------------------------------------------------------------
;; Python
;; --------------------------------------------------------------------
(use-package elpy
  :defer 6
  :init
  (use-package jedi
    :defer t)
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode)
      (add-hook 'elpy-mode-hook 'ggtags-mode)
      (add-hook 'elpy-mode-hook 'hl-todo-mode))
    (elpy-enable)
    (setq elpy-rpc-backend "jedi")))

(provide 'python-setup)
;;; python-setup ends here
