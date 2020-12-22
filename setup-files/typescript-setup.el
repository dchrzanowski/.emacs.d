;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; tide mode
;; --------------------------------------------------------------------
(use-package typescript-mode)

;; ;; --------------------------------------------------------------------
;; ;; tide mode
;; ;; --------------------------------------------------------------------
;; (use-package tide
;;   :defer 3
;;   :config
;;   (defun setup-tide-mode ()
;;     "Setup tide mode."
;;     (interactive)
;;     (tide-setup)
;;     (flycheck-mode +1)
;;     (eldoc-mode +1)
;;     (hl-todo-mode)
;;     (company-mode +1))

;;   ;; formats the buffer before saving
;;   (add-hook 'before-save-hook 'tide-format-before-save)

;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;   ;; format options
;;   (setq tide-format-options '(
;;                               :importmodulespecifierpreference "relative"
;;                               :importModuleSpecifier "relative"
;;                               :allowTextChangesInNewFiles t
;;                               :includeCompletionsForModuleExports t
;;                               :includeCompletionsWithInsertText t
;;                               :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
;;                               :placeOpenBraceOnNewLineForFunctions nil)))

(provide 'typescript-setup)
;;; typescript-setup ends here
