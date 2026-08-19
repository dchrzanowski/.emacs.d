;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; Python
;; --------------------------------------------------------------------
;; Update 29 Nov 2025
;; Replaced by lsp completely.


;; Using emacs built-in python-mode instead,
;; the one below published on gitlab seems very buggy

(use-package python-mode
  :defer 4
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode . flycheck-mode)
         (python-mode . tab-line-mode)))

;; --------------------------------------------------------------------
;; Python
;; --------------------------------------------------------------------
;; (use-package elpy
;;   :defer 4
;;   :init
;;   (use-package jedi)
;;   :config
;;   (progn
;;     ;; Use Flycheck instead of Flymake
;;     (when (require 'flycheck nil t)
;;       (remove-hook 'elpy-modules 'elpy-module-flymake)
;;       (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
;;       (add-hook 'elpy-mode-hook 'flycheck-mode)
;;       (add-hook 'elpy-mode-hook 'ggtags-mode)
;;       (add-hook 'elpy-mode-hook 'hl-todo-mode))
;;     (elpy-enable)
;;     (setq elpy-rpc-backend "jedi")))

(provide 'python-setup)
;;; python-setup.el ends here
