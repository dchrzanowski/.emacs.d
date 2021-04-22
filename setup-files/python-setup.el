;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
(use-package python-mode)

;; ;; --------------------------------------------------------------------
;; MOVING TO LSP TEMP
;; ;; --------------------------------------------------------------------
;; ;; --------------------------------------------------------------------
;; ;; Python
;; ;; --------------------------------------------------------------------
(use-package elpy
  :defer 4
  :init
  (use-package jedi)
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    ;; (when (require 'flycheck nil t)
    ;;   (remove-hook 'elpy-modules 'elpy-module-flymake)
    ;;   (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
    ;;   (add-hook 'elpy-mode-hook 'flycheck-mode)
    ;;   (add-hook 'elpy-mode-hook 'hl-todo-mode))
    (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
    (add-hook 'elpy-mode-hook 'hl-todo-mode)
    (elpy-enable)
    (setq elpy-rpc-backend "jedi")))

(provide 'python-setup)
;;; python-setup ends here
