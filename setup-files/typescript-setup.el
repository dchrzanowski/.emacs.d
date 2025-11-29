;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; typescript mode
;; --------------------------------------------------------------------
(use-package typescript-mode
  :defer 3)

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
;;     (company-mode +1)
;;     (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append))

;;   ;; formats the buffer before saving
;;   ;; (add-hook 'before-save-hook 'tide-format-before-save)

;;   ;; add tslint
;;   ;; (flycheck-add-next-checker 'typescript-tide 'typescript-tslint)
;;   ;; (flycheck-add-next-checker 'typescript-tide '(warning . typescript-tslint) 'append)

;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)

;;   ;; format options
;;   (setq tide-completion-show-source t
;;         tide-completion-detailed t
;;         tide-always-show-documentation t
;;         tide-format-options '(
;;                               :importmodulespecifierpreference "relative"
;;                               :importModuleSpecifier "relative"
;;                               :quotePreference "single"
;;                               :allowTextChangesInNewFiles t
;;                               :includeCompletionsForModuleExports t
;;                               :includeCompletionsWithInsertText t
;;                               :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
;;                               :placeOpenBraceOnNewLineForFunctions nil)))

(provide 'typescript-setup)
;;; typescript-setup.el ends here
