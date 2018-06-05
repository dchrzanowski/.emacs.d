;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; tide mode
;; --------------------------------------------------------------------
(use-package tide
  :defer 3
  :config
  (defun setup-tide-mode ()
    "Setup tide mode."
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (hl-todo-mode)
    (company-mode +1))

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  ;; format options
  (setq-default tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil)))

(provide 'typescript-setup)
;;; typescript-setup ends here
